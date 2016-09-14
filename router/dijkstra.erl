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

update(To, N, From, Sorted) ->
  Shortest = entry(To, Sorted),
  if Shortest == 0 ->
    Sorted;
    true ->
      if Shortest == inf -> replace(To, N, From, Sorted);
        true ->
          case Shortest =< N of
            true -> Sorted;
            false -> replace(To, N, From, Sorted)
          end
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
      {updateList([{Node, N} | prepare_reachable(map:reachable(Node, Map), N + 1)], Node, [], Map, T), Map, [{Node, Gateway} | Table]}
  end.

prepare_reachable(List, Distance) ->
  lists:map(fun(Elem) -> {Elem, Distance} end, List).

prepare_visited(List) ->
  lists:map(fun({Elem, _}) -> Elem end, List).

updateList(Reachable, From, Visited, Map, Sorted) ->
  if length(Reachable) == 0 -> Sorted;
    true ->
      [{Gate, Distance} | T] = Reachable,
      SortedN = update(Gate, Distance, From, Sorted),
      ReachableFromGate = prepare_reachable(map:reachable(Gate, Map), Distance + 1),
      ReachableNew = lists:foldl(
        fun(Gateway, Acc) ->
          {GatewayName, _} = Gateway,
          case lists:member(GatewayName, Visited) of
            false -> add_reachable(GatewayName, Distance + 1, Acc);
            true -> []
          end
        end, [], ReachableFromGate),
      updateList(lists:merge(T, ReachableNew), From, lists:merge(prepare_visited(ReachableNew), Visited), Map, SortedN)
  end.

add_reachable(GatewayName, Distance, Acc) -> lists:merge([{GatewayName, Distance}], Acc).

table(Gateways, Map) ->
  Sorted = handle_nodes(map:all_nodes(Map), []),
  SortedN = handle_gateways(Gateways, Sorted),
  iterate(sort(SortedN), Map, []).

handle_gateways(Gateways, Sorted) ->
  if length(Gateways) == 0 -> Sorted;
    true ->
      [H | T] = Gateways,
      handle_gateways(T, replace(H, 1, H, Sorted))
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