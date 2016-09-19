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

init(_) ->
  loop().

loop() ->
  receive
    {log, From, Time, Msg} ->

      log(From, Time, Msg),
      loop();
    stop ->
      ok
  end.

log(From, Time, Msg) ->
  io:format("log: ~w ~w ~p~n", [Time, From, Msg]).
