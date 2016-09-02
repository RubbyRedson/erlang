%%%-------------------------------------------------------------------
%%% @author Nick
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Aug 2016 4:02 PM
%%%-------------------------------------------------------------------
-module(tic).
-author("Nick").

%% API
-export([first/0]).
first() ->
  receive
    {tic, X} ->
      io:format("tic: ~w~n", [X]),
      second()
  end.

second() ->
  receive
    {tac, X} ->
      io:format("tac: ~w~n", [X]),
      last();

    {toe, X} ->
      io:format("toe: ~w~n", [X]),
      last()
  end.
last() ->
  receive
    X ->
      io:format("end: ~w~n", [X]),
      first()
  end.