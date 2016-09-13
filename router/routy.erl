%%%-------------------------------------------------------------------
%%% @author Nick
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Sep 2016 1:39 PM
%%%-------------------------------------------------------------------
-module(routy).
-author("Nick").

%% API
-export([start/2, stop/1, router/6]).

start(Reg, Name) ->
  register(Reg, spawn(fun() -> init(Name) end)).

stop(Node) ->
  Node ! stop,
  unregister(Node).

init(Name) ->
  Intf = intf:new(),
  Map = map:new(),
  Table = dijkstra:table(Intf, Map),
  Hist = hist:new(Name),
  router(Name, 0, Hist, Intf, Table, Map).

router(Name, N, Hist, Intf, Table, Map) ->
  receive

    {route, Name, From, Message} ->
      io:format("~w: received message ~w ~n", [Name, Message]),
      router(Name, N, Hist, Intf, Table, Map);

    {route, To, From, Message} ->
      io:format("~w: routing message (~w)", [Name, Message]),
      case dijkstra:route(To, Table) of
        {ok, Gw} ->
          case intf:lookup(Gw, Intf) of
            {ok, Pid} ->
              Pid ! {route, To, From, Message};
            notfound -> ok
          end;
        notfound -> ok
      end,
      router(Name, N, Hist, Intf, Table, Map);

    {send, To, Message} ->
      self() ! {route, To, Name, Message},
      router(Name, N, Hist, Intf, Table, Map);

    {add, Node, Pid} ->
      Ref = erlang:monitor(process, Pid),
      Intf1 = intf:add(Node, Ref, Pid, Intf),
      router(Name, N, Hist, Intf1, Table, Map);

    {remove, Node} ->
      case intf:ref(Node, Intf) of
        notfound -> router(Name, N, Hist, Intf, Table, Map);
        {ok, Ref} ->
          erlang:demonitor(Ref),
          Intf1 = intf:remove(Node, Intf),
          router(Name, N, Hist, Intf1, Table, Map)
      end;

    {'DOWN', Ref, process, _, _} ->
      case intf:name(Ref, Intf) of
        notfound -> router(Name, N, Hist, Intf, Table, Map);
        {ok, Down} ->
          io:format("~w: exit recived from ~w~n", [Name, Down]),
          Intf1 = intf:remove(Down, Intf),
          router(Name, N, Hist, Intf1, Table, Map)
      end;

    {links, Node, R, Links} ->
      case hist:update(Node, R, Hist) of
        {new, Hist1} ->
          intf:broadcast({links, Node, R, Links}, Intf),
          Map1 = map:update(Node, Links, Map),
          router(Name, N, Hist1, Intf, Table, Map1);
        old ->
          router(Name, N, Hist, Intf, Table, Map)
      end;

    update ->
      Table1 = dijkstra:table(intf:list(Intf), Map),
      router(Name, N, Hist, Intf, Table1, Map);

    broadcast ->
      Message = {links, Name, N, intf:list(Intf)},
      intf:broadcast(Message, Intf),
      router(Name, N + 1, Hist, Intf, Table, Map);

    {status, From} ->
      From ! {status, {Name, N, Hist, Intf, Table, Map}},
      router(Name, N, Hist, Intf, Table, Map);
    stop ->
      ok
  end.