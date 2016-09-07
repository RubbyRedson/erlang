%%%-------------------------------------------------------------------
%%% @author Nick
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Sep 2016 6:54 PM
%%%-------------------------------------------------------------------
-module(handler).
-author("Nick").

%% API
-export([handle/0]).
handle() ->
  receive
    {accept, Socket, Supervisor, Pid} ->
      request(Socket),
      Supervisor ! {free_process, Pid},
      handle()
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
  Res.