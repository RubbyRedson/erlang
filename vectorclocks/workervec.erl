%%%-------------------------------------------------------------------
%%% @author Nick
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Sep 2016 3:31 PM
%%%-------------------------------------------------------------------
-module(workervec).
-author("Nick").

%% API
-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
  spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
  Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
  random:seed(Seed, Seed, Seed),
  receive
    {peers, Peers} ->
      loop(Name, timevec:clock([{Name, self()} | Peers]), Log, Peers, Sleep, Jitter);
    stop ->
      ok
  end.

peers(Wrk, Peers) ->
  Wrk ! {peers, Peers}.

loop(Name, Time, Log, Peers, Sleep, Jitter) ->
  Wait = rand:uniform(Sleep),
  receive
    {msg, TimeRec, Msg} ->
      TimeN = timevec:inc(Name, timevec:merge(TimeRec, Time)),
      Log ! {log, Name, TimeN, {received, Msg}},
      loop(Name, TimeN, Log, Peers, Sleep, Jitter);
    stop ->
      ok;
    Error ->
      Log ! {log, Name, Time, {error, Error}}
  after Wait ->
    {_, Selected} = select(Peers),
    TimeN = timevec:inc(Name, Time),
    Message = {hello, TimeN},
    Selected ! {msg, TimeN, Message},

    jitter(Jitter),

    Log ! {log, Name, TimeN, {sending, Message}},
    loop(Name, TimeN, Log, Peers, Sleep, Jitter)
  end.

select(Peers) ->
  lists:nth(rand:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).
