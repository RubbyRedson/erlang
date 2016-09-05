%%%-------------------------------------------------------------------
%%% @author Nick
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Sep 2016 3:16 PM
%%%-------------------------------------------------------------------
-module(rudy_concurrent).
-author("Nick").

%% API
-export([init/1, request/1]).

init(Port) ->
  Opt = [list, {active, false}, {reuseaddr, true}, {keepalive, true}],
  case gen_tcp:listen(Port, Opt) of
    {ok, Listen} ->
      handler(Listen),
      gen_tcp:close(Listen),
      ok;
    {error, Error} ->
      error
  end.

handler(Listen) ->
  case gen_tcp:accept(Listen) of
    {ok, Client} ->
      Pid = spawn(rudy_concurrent, request, [Client]),
      gen_tcp:controlling_process(Client, Pid),
      handler(Listen);
    {error, Error} ->
      error
  end.

request(Client) ->
  Recv = gen_tcp:recv(Client, 0),
  case Recv of
    {ok, Str} ->
      %io:fwrite(Str),
      Request = http:parse_request(Str),
      Response = reply(Request),
      gen_tcp:send(Client, Response);
    {error, Error} ->
      io:format("rudy: error: ~w~n", [Error])
  end,
  gen_tcp:close(Client).

reply({{get, URI, _}, _, _}) ->
  timer:sleep(40), %simulate handling
  Res = http:ok(URI),
  % io:fwrite(Res),
  Res.
