%%%-------------------------------------------------------------------
%%% @author Nick
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Sep 2016 6:28 PM
%%%-------------------------------------------------------------------
-module(rudy_benchmark).
-author("Nick").

%% API
-export([bench/2]).
-export([bench_parsing/0]).
bench(Host, Port) ->
  Start = erlang:system_time(micro_seconds),
  run(100, Host, Port),
  Finish = erlang:system_time(micro_seconds),
  Finish - Start.
run(N, Host, Port) ->
  if
    N == 0 ->
      ok;
    true ->
      request(Host, Port),
      run(N-1, Host, Port)
  end.
request(Host, Port) ->
  Opt = [list, {active, false}, {reuseaddr, true}],
  {ok, Server} = gen_tcp:connect(Host, Port, Opt),
  gen_tcp:send(Server, http:get("foo")),
  Recv = gen_tcp:recv(Server, 0),
  case Recv of
    {ok, _} ->
      ok;
    {error, Error} ->
      io:format("test: error: ~w~n", [Error])
  end,
  gen_tcp:close(Server).

bench_parsing() ->
  Start = erlang:system_time(micro_seconds),
  run_parse(10000),
  Finish = erlang:system_time(micro_seconds),
  Finish - Start.


run_parse(N) ->
  if
    N == 0 ->
      ok;
    true ->
      http:parse_request(http:get("bar" ++ integer_to_list(N))),
      run_parse(N-1)
  end.
%29297000 - no optimization, rudy 4766000 4687000 4688000 4688000

%31000