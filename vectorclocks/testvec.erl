%%%-------------------------------------------------------------------
%%% @author Nick
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Sep 2016 3:33 PM
%%%-------------------------------------------------------------------
-module(testvec).
-author("Nick").

%% API
-export([run/2]).

run(Sleep, Jitter) ->
  Log = loggervec:start([john, paul, ringo, george]),
  A = workervec:start(john, Log, 13, Sleep, Jitter),
  B = workervec:start(paul, Log, 23, Sleep, Jitter),
  C = workervec:start(ringo, Log, 36, Sleep, Jitter),
  D = workervec:start(george, Log, 49, Sleep, Jitter),
  workervec:peers(A, [{paul, B}, {ringo, C}, {george, D}]),
  workervec:peers(B, [{john, A}, {ringo, C}, {george, D}]),
  workervec:peers(C, [{john, A}, {paul, B}, {george, D}]),
  workervec:peers(D, [{john, A}, {paul, B}, {ringo, C}]),
  timer:sleep(5000),
  loggervec:stop(Log),
  workervec:stop(A),
  workervec:stop(B),
  workervec:stop(C),
  workervec:stop(D).
