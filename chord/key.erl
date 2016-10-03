%%%-------------------------------------------------------------------
%%% @author Nick
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Oct 2016 12:23 PM
%%%-------------------------------------------------------------------
-module(key).
-author("Nick").

%% API
-export([generate/0, between/3]).

generate() -> rand:uniform(1000000000).

between(Key, From, To) ->
  if From == To -> true;
    true ->
      if From > To ->
        if Key > From -> true;
          true -> Key =< To
        end;
        true -> (Key > From) and (Key =< To)
      end
  end.