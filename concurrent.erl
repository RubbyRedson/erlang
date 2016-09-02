%%%-------------------------------------------------------------------
%%% @author Nick
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Aug 2016 3:56 PM
%%%-------------------------------------------------------------------
-module(concurrent).
-author("Nick").

%% API
-export([hello/0]).

hello() ->
  receive
    X -> io:format("Message received: ~s~n", [X])
  end.

