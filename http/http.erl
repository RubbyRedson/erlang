%%%-------------------------------------------------------------------
%%% @author Nick
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Sep 2016 3:00 PM
%%%-------------------------------------------------------------------
-module(http).
-author("Nick").

%% API
-export([parse_request/1]).
-export([ok/1]).
-export([get/1]).



parse_request(R0) ->
  {Request, R1} = request_line(R0),
  {Headers, R2} = headers(R1),
  {Body, _} = message_body(R2),
  {Request, Headers, Body}.

request_line([$G, $E, $T, 32 | R0]) -> %""" Problem: only parse GET requests """
  {URI, R1} = request_uri(R0),
  {Ver, R2} = http_version(R1),
  [13, 10 | R3] = R2,
  {{get, URI, Ver}, R3}.

request_uri([32|R0]) ->
  {[], R0};
request_uri([C|R0]) ->
  {Rest, R1} = request_uri(R0),
  {[C|Rest], R1}.

http_version([$H, $T, $T, $P, $/, $1, $., $1 | R0]) ->
  {v11, R0};
http_version([$H, $T, $T, $P, $/, $1, $., $0 | R0]) ->
  {v10, R0}.

headers([13,10|R0]) ->
  {[],R0};
headers(R0) ->
  {Header, R1} = header(R0),
  {Rest, R2} = headers(R1),
  {[Header|Rest], R2}.

header([13,10|R0]) ->
  {[], R0};
header([C|R0]) ->
  {Rest, R1} = header(R0),
  {[C|Rest], R1}.

message_body(R) -> %""" Problem: not correct, need to determine length of body from headers """
  {R, []}.

ok(Body) ->
  "HTTP/1.1 200 OK\r\n" ++ "\r\n" ++ Body.

get(URI) ->
  "GET " ++ URI ++ " HTTP/1.1\r\n" ++ "\r\n".
