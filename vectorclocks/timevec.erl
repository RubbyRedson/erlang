%%%-------------------------------------------------------------------
%%% @author Nick
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Sep 2016 3:39 PM
%%%-------------------------------------------------------------------
-module(timevec).
-author("Nick").

%% API
-export([zero/0, inc/2, merge/2, clock/1, update/3, safe/2, leq/2]).

zero() ->
  0.

inc(Name, T) ->
  {_, Current} = lists:keyfind(Name, 1, T),
  sort([{Name, Current + 1} | lists:keydelete(Name, 1, T)]).

merge(Ti, Tj) ->
  Result = lists:foldl(fun({Name, Time}, Acc) ->
    {_, Second} = lists:keyfind(Name, 1, Tj),
    [{Name, merge_int(Time, Second)} | Acc] end, [], Ti),
  sort(Result).

leq(Ti, Tj) ->
  Result = lists:foldl(fun({Name, Time}, Acc) ->
    {_, Second} = lists:keyfind(Name, 1, Tj),
    leq_int(Time, Second) and Acc end, true, Ti),
  Result.

merge_int(Ti, Tj) ->
  case leq_int(Ti, Tj) of
    true -> Ti;
    false -> Tj
  end.

leq_int(Ti, Tj) ->
  if Ti >= Tj -> true;
    true -> false
  end.

clock(Nodes) ->
  lists:foldl(fun({Node, _}, Acc) -> [{Node, zero()} | Acc] end, [], Nodes).

update(Node, Time, Clock) ->
  case lists:keyfind(Node, 1, Clock) of
    false -> [lists:keyfind(Node, 1, Time) | Clock];
    {_, Current} ->
      {_, Second} = lists:keyfind(Node, 1, Time),
      if Second > Current -> lists:keyreplace(Node, 1, Clock, {Node, Second});
        true -> Clock
      end
  end.
safe(Time, Clock) ->
  Res = leq(Clock, Time),
  Res.

sort(Unsorted) ->
  lists:sort(fun({N1, _}, {N2, _}) ->
    N1 =< N2 end, Unsorted).

