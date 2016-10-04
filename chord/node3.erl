%%%-------------------------------------------------------------------
%%% @author Nick
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Oct 2016 6:42 PM
%%%-------------------------------------------------------------------
-module(node3).
-author("Nick").

%% API

%% API
-compile(export_all).
-define(Stabilize, 2000).
-define(Timeout, 10000).

node(Id, Predecessor, Successor, Store, Next) ->
  io:format("node Id ~w Succ ~w Predecessor ~w Store ~w Next ~w~n", [Id, Successor, Predecessor, Store, Next]),
  receive
    {key, Qref, Peer} ->
%%      io:format("key Id ~w Qref ~w Peer ~w~n",[Id, Qref, Peer]),
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Store, Next);
    {notify, New} ->
%%      io:format("notify Id ~w New ~w~n", [Id, New]),
      {Pred, Sto} = notify(New, Id, Predecessor, Store),
      node(Id, Pred, Successor, Sto, Next);
    {request, Peer} ->
%%      io:format("request Id ~w Peer ~w~n", [Id, Peer]),
      request(Peer, Predecessor, Successor),
      node(Id, Predecessor, Successor, Store, Next);
    {status, Pred, Nx} ->
%%      io:format("status Id ~w Pred ~w~n", [Id, Pred]),
      {Succ, Nex} = stabilize(Pred, Id, Successor, Nx),
      node(Id, Predecessor, Succ, Store, Nex);
    stabilize ->
      stabilize(Successor),
      node(Id, Predecessor, Successor, Store, Next);
    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor, Store, Next);
    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor, Store, Next);
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor, Store, Next);
    {add, Key, Value, Qref, Client} ->
%%      io:format("add Id ~w Key ~w Value ~w Qref ~w Client ~w~n", [Id , Key, Value, Qref, Client]),
      Added = add(Key, Value, Qref, Client,
        Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Added, Next);
    {lookup, Key, Qref, Client} ->
      lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Store, Next);
    {handover, Elements} ->
%%      io:format("handover Id ~w Elements ~w~n", [Id, Elements]),
      Merged = storage:merge(Store, Elements),
%%      io:format("handover after merge Id ~w Merged ~w~n", [Id, Merged]),
      node(Id, Predecessor, Successor, Merged, Next);
    {'DOWN', Ref, process, _, _} ->
      {_, Sref, _} = Successor,
      if Ref == Sref ->
        {Pred, Succ, Nxt} = down_succ(Ref, Predecessor, Successor, Next),
        node(Id, Pred, Succ, Store, Nxt);
        true ->
          {Pred, Succ, Nxt} = down_pred(Ref, Predecessor, Successor, Next),
          node(Id, Pred, Succ, Store, Nxt)
      end;
    stop -> ok
  end.

stabilize(Pred, Id, Successor, Next) ->
  {Skey, Sref, Spid} = Successor,
%%  io:format("stabilize Pred ~w Id ~w Succ ~w~n",[Pred, Id, Successor]),
  case Pred of
    nil ->
%%      io:format("stabilize nil ~w Id ~w Succ ~w~n",[Pred, Id, Successor]),
      Spid ! {notify, {Id, self()}},
      {{Skey, Sref, Spid}, Next};
    {Id, _} ->
%%      io:format("stabilize {Id, _} Pred ~w Id ~w Succ ~w~n",[Pred, Id, Successor]),
      {{Skey, Sref, Spid}, Next};
    {Skey, _} ->
%%      io:format("stabilize {Skey, _} ~w Id ~w Succ ~w~n",[Pred, Id, Successor]),
      Spid ! {notify, {Id, self()}},
      {{Skey, Sref, Spid}, Next};
    {Xkey, Xpid} ->
      case key:between(Xkey, Id, Skey) of
        true ->
%%          io:format("stabilize {Xkey, Xpid} true ~w Id ~w Succ ~w~n",[Pred, Id, Successor]),
          {Pid, Pprocid} = Pred,
          drop(Sref),
          {{Pid, monitor(Pprocid), Pprocid}, {Skey, Spid}};
        false ->
%%          io:format("stabilize {Xkey, Xpid} false ~w Id ~w Succ ~w~n",[Pred, Id, Successor]),
          self() ! {notify, {Xkey, Xpid}},
          {{Skey, Sref, Spid}, Next}
      end
  end.

schedule_stabilize() ->
  timer:send_interval(?Stabilize, self(), stabilize).

stabilize({_, _, Spid}) ->
  Spid ! {request, self()}.

request(Peer, Predecessor, {SuccId, _, SuccP}) ->
  case Predecessor of
    nil ->
      Peer ! {status, nil, {SuccId, SuccP}};
    {Pkey, _, Ppid} ->
      Peer ! {status, {Pkey, Ppid}, {SuccId, SuccP}}
  end.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
  case Predecessor of
    nil ->
      Keep = handover(Id, Store, Nkey, Npid),
      {{Nkey, monitor(Npid), Npid}, Keep};
    {Pkey, Pref, Ppid} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          Keep = handover(Id, Store, Nkey, Npid),
          drop(Pref),
          {{Nkey, monitor(Npid), Npid}, Keep};
        false ->
          Npid ! {status, {Pkey, Ppid}},
          {Predecessor, Store}
      end
  end.

handover(Id, Store, Nkey, Npid) ->
  io:format("handover ~w Store ~w Nkey ~w Npid ~w  ~n", [Id, Store, Nkey, Npid]),
  {Keep, Rest} = storage:split(Id, Nkey, Store),
%%  io:format("handover  ~w after Keep ~w Rest ~w  ~n", [Id, Keep, Rest]),
  Npid ! {handover, Rest},
  Keep.


start(Id) ->
  start(Id, nil).

start(Id, Peer) ->
  timer:start(),
  spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
  Predecessor = nil,
  {ok, Successor} = connect(Id, Peer),
  schedule_stabilize(),
  {Skey, Spid} = Successor,
  node(Id, Predecessor, {Skey, monitor(Spid), Spid}, [], nil).

connect(Id, nil) ->
  {ok, {Id, self()}};

connect(Id, Peer) ->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} ->
      {ok, {Skey, Peer}}
  after ?Timeout ->
    io:format("Time out: no response~n", [])
  end.

add(Key, Value, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
%%      io:format("add true Key ~w, Value ~w, Qref ~w Client ~w Id ~w Pkey ~w Store ~w ~n",[Key, Value, Qref, Client, Id, Pkey, Store]),
      Client ! {Qref, ok},
      [{Key, Value} | Store];
    false ->
%%      io:format("add false Key ~w, Value ~w, Qref ~w Client ~w Id ~w Pkey ~w Store ~w ~n",[Key, Value, Qref, Client, Id, Pkey, Store]),
      Spid ! {add, Key, Value, Spid, Client},
      Store
  end.

lookup(Key, Qref, Client, Id, {Pkey, _, _}, Successor, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      Result = storage:lookup(Key, Store),
%%      io:format("lookup true Key ~w, Qref ~w Client ~w Id ~w Pkey ~w Store ~w Result ~w ~n",[Key, Qref, Client, Id, Pkey, Store, Result]),
      Client ! {Qref, Result};
    false ->
%%      io:format("lookup false Key ~w, Qref ~w Client ~w Id ~w Pkey ~w Store ~w ~n",[Key, Qref, Client, Id, Pkey, Store]),
      {_, _, Spid} = Successor,
      Spid ! {lookup, Key, Qref, Client}
  end.


create_probe(Id, {Successor, _, SuccId}) ->
  SuccId ! {probe, Id, [Id], erlang:system_time(micro_seconds)}.

remove_probe(T, Nodes) ->
  io:fwrite("Probe returned Nodes ~w Time ~w~n", [Nodes, T]).

forward_probe(Ref, T, Nodes, Id, {Successor, _, SuccId}) ->
  case lists:member(Successor, Nodes) of
    true ->
%%          io:format("forward_probe true Ref ~w, T ~w, Nodes ~w, Id ~w, {Successor, SuccId} ~w~n",[Ref, T, Nodes, Id, {Successor, SuccId}]),
      SuccId ! {probe, Ref, [Id | Nodes], erlang:system_time(micro_seconds)};
    false ->
%%      io:format("forward_probe false Ref ~w, T ~w, Nodes ~w, Id ~w, {Successor, SuccId} ~w~n", [Ref, T, Nodes, Id, {Successor, SuccId}]),
      SuccId ! {probe, Id, [Id | Nodes], erlang:system_time(micro_seconds)}
  end.

monitor(Pid) ->
  erlang:monitor(process, Pid).

drop(nil) ->
  ok;

drop(Pid) ->
  erlang:demonitor(Pid, [flush]).

down_pred(Ref, {_, Ref, _}, Successor, Next) ->
  {nil, Successor, Next}.

down_succ(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid}) ->
  {Predecessor, {Nkey, monitor(Npid), Npid}, nil}.

