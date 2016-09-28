%%%-------------------------------------------------------------------
%%% @author Nick
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Sep 2016 3:29 PM
%%%-------------------------------------------------------------------
-module(loggervec).
-author("Nick").

%% API
-export([start/1, stop/1]).

start(Nodes) ->
  spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
  Logger ! stop.

init(Nodes) ->
  Clock = time:clock(Nodes),
  loop(Clock, []).

loop(Clock, Queue) ->
  receive
    {log, From, Time, Msg} ->
      ClockN = timevec:update(From, Time, Clock),
      Queued = sort([{From, Time, Msg} | Queue]),
      QueueN = lists:foldl(
        fun(Elem, Acc) ->
          {FromN, TimeN, MsgN} = Elem,
          case timevec:safe(TimeN, ClockN) of
            true -> log(FromN, TimeN, MsgN), Acc;
            false -> [{FromN, TimeN, MsgN} | Acc]
          end
        end, [], Queued),
      loop(ClockN, QueueN);
    stop ->
      ok
  end.

log(From, Time, Msg) ->
  io:format("log: ~w ~w ~w~n", [Time, From, Msg]).

sort(Unsorted) ->
  Res = lists:sort(fun({_, N1, _}, {_, N2, _}) ->
    timevec:leq(N2, N1) end, Unsorted),
  Res.

