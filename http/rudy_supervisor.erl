%%%-------------------------------------------------------------------
%%% @author Nick
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Sep 2016 7:22 PM
%%%-------------------------------------------------------------------
-module(rudy_supervisor).
-author("Nick").

%% API
-export([init/0]).

init() ->
  listen(init_handlers(50)).

listen(Handlers) ->
  receive
    {free_process, Pid} ->
      listen(add_handler(Pid, Handlers));
    {socket, Client, Supervisor} ->
      if length(Handlers) > 0 ->
        {Pid, Tmp} = get_handler(Handlers),
        Pid ! {accept, Client, Supervisor, Pid},
        listen(Tmp);
        true ->
          Supervisor ! {socket, Client, Supervisor},
          listen(Handlers)
      end
  end.

init_handlers(Number_of_handlers) ->
  [init_handler() || _ <- lists:seq(1, Number_of_handlers)].

init_handler() ->
  spawn(handler, handle, []).

add_handler(Pid, Handlers) ->
  [Pid | Handlers].

get_handler([H|T]) ->
  {H, T}.

%rudy_benchmark:bench("192.168.1.174", 8080).
%rudy_benchmark:bench_concurrent("130.229.159.8", 8080).
%rudy_benchmark:bench_concurrent("192.168.1.174", 8080).