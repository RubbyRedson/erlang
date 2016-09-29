%%%-------------------------------------------------------------------
%%% @author Nick
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Sep 2016 10:54 AM
%%%-------------------------------------------------------------------
-module(gms4).
-author("Nick").

%% API
-compile(export_all).
-define(timeout, 1000).
-define(arghh, 25).


leader(Id, Master, N, Slaves, Group) ->
  receive
    {mcast, Msg} ->
      io:format("leader mcast ~w~n", [Msg]),
      bcast(Id, {msg, N + 1, Msg}, Slaves),
      Master ! Msg,
      leader(Id, Master, N + 1, Slaves, Group);
    {join, Wrk, Peer} ->
      io:format("leader join~n"),
      Slaves2 = lists:append(Slaves, [Peer]),
      Group2 = lists:append(Group, [Wrk]),
      bcast_no_ack(Id, {view, [self() | Slaves2], Group2}, Slaves2),
      Master ! {view, Group2},
      leader(Id, Master, N + 1, Slaves2, Group2);
    stop ->
      ok
  end.

slave(Id, Master, Leader, N, Last, Slaves, Group) ->
  receive
    {mcast, Msg} ->
      io:format("slave mcast ~w Leader ~w Slaves ~w Group ~w~n", [Msg, Leader, Slaves, Group]),
      Leader ! {mcast, Msg},
      Leader ! {ok, self(), {mcast, Msg}},
      slave(Id, Master, Leader, N, Msg, Slaves, Group);
    {join, Wrk, Peer} ->
      io:format("slave join~n"),
      Leader ! {join, Wrk, Peer},
      Leader ! {ok, self(), {join, Wrk, Peer}},
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {msg, I, Content} when I < N ->
      io:format("slave I<N I ~w N ~w~n", [I, N]),
      Leader ! {ok, self(), {msg, I, Content}},
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {msg, MsgNum, Msg} ->
      case random:uniform(?arghh) of
        ?arghh -> io:format("slave ~w Msg ~w: drop it like it's hot~n", [Id, Msg]);
        _ ->
          Leader ! {ok, self(), {msg, MsgNum, Msg}},
          Master ! Msg
      end,
      io:format("slave msg ~w MsgNum ~w N ~w~n", [Msg, MsgNum, N]),
      slave(Id, Master, Leader, MsgNum, Msg, Slaves, Group);
    {view, [Leader | Slaves2], Group2} ->
      io:format("slave ~w view ~w ~n", [self(), {view, [Leader | Slaves2], Group2}]),
      Leader ! {ok, self(), {view, [Leader | Slaves2], Group2}},
      Master ! {view, Group2},
      slave(Id, Master, Leader, N, Last, Slaves2, Group2);
    {'DOWN', _Ref, process, Leader, _Reason} ->
      election(Id, Master, N, Last, Slaves, Group);
    stop ->
      ok
  after ?timeout ->
    Master ! {error, "no reply from leader"}
  end.

start(Id) ->
  Self = self(),
  {ok, spawn_link(fun() -> init(Id, Self) end)}.

init(Id, Master) ->
  leader(Id, Master, 0, [], [Master]).

start(Id, Grp) ->
  Self = self(),
  {ok, spawn_link(fun() -> init(Id, Grp, Self) end)}.

init(Id, Grp, Master) ->
  Self = self(),
  Grp ! {join, Master, Self},
  receive
    {view, [Leader | Slaves], Group} ->
      Master ! {view, Group},
      erlang:monitor(process, Leader),
      slave(Id, Master, Leader, 0, {masg, 0, 1}, Slaves, Group)
  end.


bcast(Id, Msg, Nodes) ->
  lists:foreach(fun(Node) ->
    io:format("send Node ~w Msg ~w~n", [Node, Msg]),
    Node ! Msg,
    crash(Id)
                end, Nodes),
  receiveAck(Nodes, Msg).

bcast_no_ack(Id, Msg, Nodes) ->
  lists:foreach(fun(Node) ->
    io:format("send Node ~w Msg ~w~n", [Node, Msg]),
    Node ! Msg,
    crash(Id)
                end, Nodes).

receiveAck(Nodes, Msg) ->
  if length(Nodes) == 0 -> ok;
    true ->
      receive
        {ok, Node, RecMsg} when Msg == RecMsg ->
          io:format("acknowledged Node ~w Msg ~w~n", [Node, Msg]),
          io:format("lists old ~w new ~w~n", [lists:delete(Node, Nodes), Nodes]),
          receiveAck(lists:delete(Node, Nodes), Msg),
          ok
%%      ;
%%        Rec ->
%%          io:format("received Nodes ~w Msg ~w Rec ~w~n", [Nodes, Msg, Rec]), self() ! Rec
      after ?timeout ->
        lists:foreach(fun(Node) ->
          io:format("resend Node ~w Msg ~w~n", [Node, Msg]),
          Node ! Msg end, Nodes),
        receiveAck(Nodes, Msg)
      end end.

crash(Id) ->
  case random:uniform(?arghh) of
    ?arghh -> ok;
%%      io:format("leader ~w: crash~n", [Id]),
%%      exit(no_luck);
    _ ->
      ok
  end.


election(Id, Master, N, Last, Slaves, [_ | Group]) ->
%%  io:format("election ~w N ~w Last ~w~n", [self(), N, Last]),
  Self = self(),
  case Slaves of
    [Self | Rest] ->
      uniform(N, Last, Slaves),
      bcast(Id, {view, Slaves, Group}, Rest),
      Master ! {view, Group},
      self() ! {mcast, Last},
      leader(Id, Master, N, Rest, Group);
    [Leader | Rest] ->
      erlang:monitor(process, Leader),
      Leader ! {mcast, Last},
      slave(Id, Master, Leader, N, Last, Rest, Group)
  end.

uniform(N, Msg, All) ->
  lists:foreach(fun(Elem) -> Elem ! {msg, N - 1, Msg} end, All).