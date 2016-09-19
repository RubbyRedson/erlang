%%%-------------------------------------------------------------------
%%% @author Nick
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Sep 2016 3:29 PM
%%%-------------------------------------------------------------------
-module(logger).
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
      ClockN = time:update(From, Time, Clock),
%%      io:format("logger From ~w Time ~w Msg ~w Clock ~w Queue ~w~n", [From, Time, Msg, ClockN, Queue]),
      Queued = sort([{From, Time, Msg} | Queue]),
      case time:safe(Time, ClockN) of
        true ->
          QueueN = lists:foldl(
            fun(Elem, Acc) ->
              {FromN, TimeN, MsgN} = Elem,
              case time:safe(TimeN, ClockN) of
                true -> log(FromN, TimeN, MsgN), Acc;
                false -> [{FromN, TimeN, MsgN} | Acc]
              end
            end, [], Queued),
%%          io:format("logger QueueN ~w~n", [QueueN]),
          loop(ClockN, sort(QueueN));
        false -> loop(ClockN, Queued)
      end;
    stop ->
      ok
  end.

log(From, Time, Msg) ->
  io:format("log: ~w ~w ~p~n", [Time, From, Msg]).

sort(Unsorted) ->
  lists:sort(fun({_, N1, _}, {_, N2, _}) -> N1 =< N2 end, Unsorted).