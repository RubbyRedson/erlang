%%%-------------------------------------------------------------------
%%% @author Nick
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Sep 2016 12:08 PM
%%%-------------------------------------------------------------------
-module(hist).
-author("Nick").

%% API
-export([new/1, update/3]).

new(Name) ->
  [{Name, none}].

update(Node, N, History) ->
  case lists:keyfind(Node, 1, History) of
    false -> {new, [{Node, N} | History]};
    {_, Current} ->
      if Current == none -> {new, lists:keyreplace(Node, 1, History, {Node, N})};
        true ->
          if Current >= N -> old;
            true -> {new, lists:keyreplace(Node, 1, History, {Node, N})}
          end
      end
  end.