%%%-------------------------------------------------------------------
%%% @author Nick
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Sep 2016 4:03 PM
%%%-------------------------------------------------------------------
-module(higherorder).
-author("Nick").

%% API
-export([map/2]).

map(Fun, List) ->
  case List of
    [] ->
      [];
    [H | T] ->
      [Fun(H) | map(Fun, T)]
  end.

%"""higherorder:map(fun(X) -> io:fwrite("~w~n", [X]) end, [1, 2, 3])."""
%"""higherorder:map(fun(X) -> X*2 end, [1, 2, 3])."""