%%%-------------------------------------------------------------------
%%% @author Nick
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Oct 2016 10:25 AM
%%%-------------------------------------------------------------------
-module(storage).
-author("Nick").

%% API
-compile(export_all).

merge(Store, Elements) ->
  Store1 = lists:foldl(fun({Key, _}, Acc) ->
    lists:keydelete(Key, 1, Acc) end, Store, Elements),
  lists:foldl(fun({Key, Value}, Acc) ->
    [{Key, Value} | Acc] end, Store1, Elements).

split(Id, Nkey, Store) ->
  lists:partition(fun({Key, _}) -> (Key >= Id) and (Key < Nkey) end, Store).

lookup(Key, Store) ->
  case lists:keyfind(Key, 1, Store) of
    false -> io:format("Not found!! ~w in ~w~n", [Key, Store]), notfound;
    {_, Value} -> Value
  end.