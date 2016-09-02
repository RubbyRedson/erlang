%%%-------------------------------------------------------------------
%%% @author Nick
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Aug 2016 3:41 PM
%%%-------------------------------------------------------------------
-module(tmp).
-author("Nick").

%% API
-export([tmp_func/1]).
tmp_func(Name) ->
  io:fwrite("Hey ~s\n", [Name]).