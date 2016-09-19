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
  lists:foldl(fun(Node, Acc) -> [{Node, 0} | Acc] end, [], Nodes).

update(Node, Time, Clock) ->
  case lists:keyfind(Node, 1, Clock) of
    false -> [{Node, Time} | Clock];
    {_, Current} ->
      if Current == none -> {new, lists:keyreplace(Node, 1, Clock, {Node, Time})};
        true ->
          if Current >= Time -> Clock;
            true -> {new, lists:keyreplace(Node, 1, Clock, {Node, Time})}
          end
      end
  end.

safe(Time, Clock) ->
  Max = get_max(Clock),
  if Time >= Max -> true;
    true -> false
  end.


get_max(Clock) ->
  if length(Clock) == 0 -> 0;
    true ->
      [H | T] = Clock,
      max(H, get_max(T))
  end.