%%%-------------------------------------------------------------------
%%% @author Nick
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Sep 2016 3:52 PM
%%%-------------------------------------------------------------------
-module(start).
-author("Nick").

%% API
-export([start/1, stop/0]).

start(Port) ->
  register(rudy, spawn(fun() -> rudy:init(Port) end)).
stop() ->
  exit(whereis(rudy), "time to die").