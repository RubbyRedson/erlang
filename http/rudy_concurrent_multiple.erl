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
-export([init/1, init_handler/0]).

init_handlers(Number_of_handlers) ->
  [init_handler() || _ <- lists:seq(1, Number_of_handlers)].

init_handler() ->
  spawn(handler, handle, []).

add_handler(List) ->
  [spawn(handler, handle, []) | List].

init(Port) ->
  Opt = [list, {active, false}, {reuseaddr, true}, {keepalive, true}],
  Handlers = init_handlers(10),
  listen(Port, Opt, Handlers).


listen(Port, Opt, Handlers) ->
  case gen_tcp:listen(Port, Opt) of
    {ok, Listen} ->
      process(Listen, Handlers);
    {error, Error} ->
      io:fwrite(Error),
      error
  end.

process(Listen, Handlers) ->
  case gen_tcp:accept(Listen) of
    {ok, Client} ->
      Pid = lists:last(Handlers),
      Tmp = lists:reverse(tl(lists:reverse(Handlers))),
      Pid ! {accept, Client},
      rudy_concurrent_multiple:init_handler(),
      process(Listen, add_handler(Tmp));
    {error, Error} ->
      error
  end.