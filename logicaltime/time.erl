%%%-------------------------------------------------------------------
%%% @author Nick
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Sep 2016 3:39 PM
%%%-------------------------------------------------------------------
-module(time).
-author("Nick").

%% API
-export([zero/0, inc/2, merge/2, clock/1, update/3, safe/2]).

zero() ->
  0.

inc(Name, T) ->
  T + 1.

merge(Ti, Tj) ->
  case leq(Ti, Tj) of
    true -> Ti;
    false -> Tj
  end.

leq(Ti, Tj) ->
  if Ti >= Tj -> true;
    true -> false
  end.

clock(Nodes) ->
  lists:foldl(fun(Node, Acc) -> [{Node, zero()} | Acc] end, [], Nodes).

update(Node, Time, Clock) ->
  case lists:keyfind(Node, 1, Clock) of
    false -> [{Node, Time} | Clock];
    {_, Current} ->
      if Time > Current -> lists:keyreplace(Node, 1, Clock, {Node, Time});
        true -> Clock
      end
  end.

safe(Time, Clock) ->
  Min = get_min(Clock),
%%  io:format("Check safe; Time ~w Min ~w Clock ~w~n", [Time, Min, Clock]),
  if Time =< Min -> true;
    true -> false
  end.


get_min(Clock) ->
  if length(Clock) == 0 -> inf;
    true ->
%%      io:format("get_max Clock ~w~n", [Clock]),
      [{_, Time} | T] = Clock,
      min(Time, get_min(T))
  end.