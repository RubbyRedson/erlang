%%%-------------------------------------------------------------------
%%% @author Nick
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Sep 2016 6:16 PM
%%%-------------------------------------------------------------------
-module(rudy_concurrent_multiple).
-author("Nick").
%% API
-export([init/1]).

init(Port) ->
  Opt = [list, {active, false}, {reuseaddr, true}, {keepalive, true}],
  listen(Port, Opt).


listen(Port, Opt) ->
  case gen_tcp:listen(Port, Opt) of
    {ok, Listen} ->
      process(Listen, spawn(rudy_supervisor, init, []));
    {error, Error} ->
      io:fwrite(Error),
      error
  end.

process(Listen, Supervisor) ->
  case gen_tcp:accept(Listen) of
    {ok, Client} ->
      Supervisor ! {socket, Client, Supervisor},
      process(Listen, Supervisor);
    {error, Error} ->
      error
  end.