%%%-------------------------------------------------------------------
%%% @author Nick
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Sep 2016 2:09 PM
%%%-------------------------------------------------------------------
-module(dijkstra).
-author("Nick").

%% API
-export([table/2, route/2]).

entry(Node, Sorted) ->
  case lists:keyfind(Node, 1, Sorted) of
    false -> 0;
    {_, Length, _} -> Length
  end.

replace(Node, N, Gateway, Sorted) ->
  sort(lists:keystore(Node, 1, Sorted, {Node, N, Gateway})).

update(Node, N, Gateway, Sorted) ->
  Shortest = entry(Node, Sorted),
  if Shortest == 0 ->
    Sorted;
    true ->
      case Shortest =< N of
        true -> Sorted;
        false -> replace(Node, N, Gateway, Sorted)
      end
  end.

sort(Unsorted) ->
  lists:sort(fun({_, N1, _}, {_, N2, _}) -> N1 =< N2 end, Unsorted).

iterate(Sorted, Map, Table) ->
  if length(Sorted) == 0 -> Table;
    true ->
      {SortedN, MapN, TableN} = handle(Sorted, Map, Table),
      iterate(SortedN, MapN, TableN)
  end.

handle([{Node, N, Gateway} | T], Map, Table) ->
  if N == inf -> {[], Map, Table};
    true ->
      {updateList(map:reachable(Node, Map), Node, T), Map, [{Node, Gateway} | Table]}
  end.

updateList(Reachable, Node, Sorted) ->
  if length(Reachable) == 0 -> Sorted;
    true ->
      [H | T] = Reachable,
      SortedN = update(H, 1, Node, Sorted),
      updateList(T, Node, SortedN)
  end.

table(Gateways, Map) ->
  Sorted = handle_nodes(map:all_nodes(Map), []),
  SortedN = handle_gateways(Gateways, Sorted),
  iterate(sort(SortedN), Map, []).

handle_gateways(Gateways, Sorted) ->
  if length(Gateways) == 0 -> Sorted;
    true ->
      [H | T] = Gateways,
      handle_gateways(T, update(H, 0, H, Sorted))
  end.

handle_nodes(Nodes, Sorted) ->
  if length(Nodes) == 0 -> Sorted;
    true ->
      [H | T] = Nodes,
      handle_nodes(T, replace(H, inf, unknown, Sorted))
  end.

route(Node, Table) ->
  case lists:keyfind(Node, 1, Table) of
    false -> notfound;
    {_, Gateway} -> {ok, Gateway}
  end.