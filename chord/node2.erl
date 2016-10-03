%%%-------------------------------------------------------------------
%%% @author Nick
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Oct 2016 12:23 PM
%%%-------------------------------------------------------------------
-module(node2).
-author("Nick").

%% API
-compile(export_all).
-define(Stabilize, 2000).
-define(Timeout, 10000).

node(Id, Predecessor, Successor, Store) ->
  io:format("node Id ~w Succ ~w Predecessor ~w~n", [Id, Successor, Predecessor]),
  receive
    {key, Qref, Peer} ->
%%      io:format("key Id ~w Qref ~w Peer ~w~n",[Id, Qref, Peer]),
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Store);
    {notify, New} ->
      io:format("notify Id ~w New ~w~n", [Id, New]),
      {Pred, Sto} = notify(New, Id, Predecessor, Store),
      node(Id, Pred, Successor, Sto);
    {request, Peer} ->
      io:format("request Id ~w Peer ~w~n", [Id, Peer]),
      request(Peer, Predecessor),
      node(Id, Predecessor, Successor, Store);
    {status, Pred} ->
      io:format("status Id ~w Pred ~w~n", [Id, Pred]),
      Succ = stabilize(Pred, Id, Successor),
      node(Id, Predecessor, Succ, Store);
    stabilize ->
      stabilize(Successor),
      node(Id, Predecessor, Successor, Store);
    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor, Store);
    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor, Store);
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor, Store);
    {add, Key, Value, Qref, Client} ->
      io:format("add Id ~w Key ~w Value ~w Qref ~w Client ~w~n", [Id , Key, Value, Qref, Client]),
      Added = add(Key, Value, Qref, Client,
        Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Added);
    {lookup, Key, Qref, Client} ->
      lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Store);
    {handover, Elements} ->
      io:format("handover Id ~w Elements ~w~n", [Id, Elements]),
      Merged = storage:merge(Store, Elements),
      node(Id, Predecessor, Successor, Merged)
  end.

stabilize(Pred, Id, Successor) ->
  {Skey, Spid} = Successor,
%%  io:format("stabilize Pred ~w Id ~w Succ ~w~n",[Pred, Id, Successor]),
  case Pred of
    nil ->
%%      io:format("stabilize nil ~w Id ~w Succ ~w~n",[Pred, Id, Successor]),
      Spid ! {notify, {Id, self()}},
      Successor;
    {Id, _} ->
%%      io:format("stabilize {Id, _} Pred ~w Id ~w Succ ~w~n",[Pred, Id, Successor]),
      Successor;
    {Skey, _} ->
%%      io:format("stabilize {Skey, _} ~w Id ~w Succ ~w~n",[Pred, Id, Successor]),
      Spid ! {notify, {Id, self()}},
      Successor;
    {Xkey, Xpid} ->
      case key:between(Xkey, Id, Skey) of
        true ->
%%          io:format("stabilize {Xkey, Xpid} true ~w Id ~w Succ ~w~n",[Pred, Id, Successor]),
          Pred;
        false ->
%%          io:format("stabilize {Xkey, Xpid} false ~w Id ~w Succ ~w~n",[Pred, Id, Successor]),
          Spid ! {notify, {Xkey, Xpid}},
          Successor
      end
  end.

schedule_stabilize() ->
  timer:send_interval(?Stabilize, self(), stabilize).

stabilize({_, Spid}) ->
  Spid ! {request, self()}.

request(Peer, Predecessor) ->
  case Predecessor of
    nil ->
      Peer ! {status, nil};
    {Pkey, Ppid} ->
      Peer ! {status, {Pkey, Ppid}}
  end.

%%notify({Nkey, Npid}, Id, Predecessor) ->
%%  case Predecessor of
%%    nil ->
%%%%  io:format("notify nil Pred ~w Id ~w New ~w~n",[Predecessor, Id, {Nkey, Npid}]),
%%      {Nkey, Npid};
%%    {Pkey, _} ->
%%      case key:between(Nkey, Pkey, Id) of
%%        true ->
%%%%          io:format("notify true Pred ~w Id ~w New ~w~n",[Predecessor, Id, {Nkey, Npid}]),
%%          {Nkey, Npid};
%%        false ->
%%%%          io:format("notify false Pred ~w Id ~w New ~w~n",[Predecessor, Id, {Nkey, Npid}]),
%%          Npid ! {status, Predecessor},
%%          Predecessor
%%      end
%%  end.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
  case Predecessor of
    nil ->
      Keep = handover(Id, Store, Nkey, Npid),
      {{Nkey, Npid}, Keep};
    {Pkey, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          Keep = handover(Id, Store, Nkey, Npid),
          {{Nkey, Npid}, Keep};
        false ->
          Npid ! {status, Predecessor},
          {Predecessor, Store}
      end
  end.

handover(Id, Store, Nkey, Npid) ->
  {Keep, Rest} = storage:split(Id, Nkey, Store),
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
  node(Id, Predecessor, Successor, []).

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

add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
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

lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      Result = storage:lookup(Key, Store),
      io:format("lookup true Key ~w, Qref ~w Client ~w Id ~w Pkey ~w Store ~w Result ~w ~n",[Key, Qref, Client, Id, Pkey, Store, Result]),
      Client ! {Qref, Result};
    false ->
      io:format("lookup false Key ~w, Qref ~w Client ~w Id ~w Pkey ~w Store ~w ~n",[Key, Qref, Client, Id, Pkey, Store]),
      {_, Spid} = Successor,
      Spid ! {lookup, Key, Qref, Client}
  end.


create_probe(Id, {Successor, SuccId}) ->
  SuccId ! {probe, Id, [Id], erlang:system_time(micro_seconds)}.

remove_probe(T, Nodes) ->
  io:fwrite("Probe returned Nodes ~w Time ~w~n", [Nodes, T]).

forward_probe(Ref, T, Nodes, Id, {Successor, SuccId}) ->
  case lists:member(Successor, Nodes) of
    true ->
%%          io:format("forward_probe true Ref ~w, T ~w, Nodes ~w, Id ~w, {Successor, SuccId} ~w~n",[Ref, T, Nodes, Id, {Successor, SuccId}]),
      SuccId ! {probe, Ref, [Id | Nodes], erlang:system_time(micro_seconds)};
    false ->
%%      io:format("forward_probe false Ref ~w, T ~w, Nodes ~w, Id ~w, {Successor, SuccId} ~w~n", [Ref, T, Nodes, Id, {Successor, SuccId}]),
      SuccId ! {probe, Id, [Id | Nodes], erlang:system_time(micro_seconds)}
  end.