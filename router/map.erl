%%%-------------------------------------------------------------------
%%% @author Nick
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Sep 2016 1:16 PM
%%%-------------------------------------------------------------------
-module(map).
-author("Nick").

%% API
-export([new/0, update/3, reachable/2, all_nodes/1]).


new() ->
  [].

update(Node, Links, Map) ->
  lists:keystore(Node, 1, Map, {Node, Links}).

reachable(Node, Map) ->
  case lists:keyfind(Node, 1, Map) of
    {_, Links} -> Links;
    false -> []
  end.

all_nodes(Map) ->
  List = lists:foldl(fun({Node, Links}, Acc) -> [lists:merge([Node], Links) | Acc] end, [], Map),
  sets:to_list(sets:from_list(lists:merge(List))).