%%%-------------------------------------------------------------------
%%% @author Nick
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Sep 2016 10:54 AM
%%%-------------------------------------------------------------------
-module(gms3).
-author("Nick").

%% API
-compile(export_all).
-define(timeout, 5000).
-define(arghh, 100).


leader(Id, Master, N, Slaves, Group) ->
  receive
    {mcast, Msg} ->
%%      io:format("leader mcast ~w~n", [Msg]),
      bcast(Id, {msg, N + 1, Msg}, Slaves),
      Master ! Msg,
      leader(Id, Master, N + 1, Slaves, Group);
    {join, Wrk, Peer} ->
%%      io:format("leader join~n"),
      Slaves2 = lists:append(Slaves, [Peer]),
      Group2 = lists:append(Group, [Wrk]),
      bcast(Id, {view, [self() | Slaves2], Group2}, Slaves2),
      Master ! {view, Group2},
      leader(Id, Master, N + 1, Slaves2, Group2);
    stop ->
      ok
  end.

slave(Id, Master, Leader, N, Last, Slaves, Group) ->
  receive
    {mcast, Msg} ->
%%      io:format("slave mcast ~w Leader ~w Slaves ~w Group ~w~n", [Msg, Leader, Slaves, Group]),
      Leader ! {mcast, Msg},
      slave(Id, Master, Leader, N, Msg, Slaves, Group);
    {join, Wrk, Peer} ->
%%      io:format("slave join~n"),
      Leader ! {join, Wrk, Peer},
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {msg, I, _} when I < N ->
%%      io:format("slave I<N I ~w N ~w~n", [I, N]),
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {msg, MsgNum, Msg} ->
%%      io:format("slave msg ~w MsgNum ~w N ~w~n", [Msg, MsgNum, N]),
      Master ! Msg,
      slave(Id, Master, Leader, MsgNum, Msg, Slaves, Group);
    {view, [Leader | Slaves2], Group2} ->
%%      io:format("slave view~n"),
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
  lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).
crash(Id) ->
  case random:uniform(?arghh) of
    ?arghh ->
      io:format("leader ~w: crash~n", [Id]),
      exit(no_luck);
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
  lists:foreach(fun(Elem) -> Elem ! {msg, N - 1, Msg}end, All).