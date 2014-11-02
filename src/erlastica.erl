-module(erlastica).
-author("marcelog@gmail.com").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([cfg_get/1]).
-export([start/0]).
-export([index_host_config/1]).
-export([delete_index/1]).
-export([create_index/2]).
-export([index/3, index/4]).
-export([scan/4, scan/5]).
-export([find/2, find_by_id/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Types.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type ejson_property():: {atom() | string() | binary(), ejson()}.
-type ejson()::
  tuple(list(ejson_property()))
  | binary()
  | true
  | false
  | null
  | number()
  | [ejson()].
-export_type([ejson/0]).

-type result()::
  {ok | error, egetter:status(), [egetter:header()], ejson()}.
-export_type([result/0]).

-type index():: atom().
-type type():: string().
-type doc():: ejson().
-type querydoc():: ejson().
-export_type([index/0, type/0, doc/0]).

-type index_option()::
  {version, pos_integer()}
  | {id, string()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constants.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(APPS, [
  compiler,
  syntax_tools,
  lager,
  crypto,
  asn1,
  public_key,
  ssl,
  ibrowse,
  egetter,
  erlastica
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Useful to start the application via the -s command line argument.
-spec start() -> ok.
start() ->
  _ = [application:start(A) || A <- ?APPS],
  ok.

%% @doc Retrieves a configuration key from the application environment or the
%% given default value.
-spec cfg_get(atom(), term()) -> term().
cfg_get(Key, Default) ->
  case cfg_get(Key) of
    undefined -> Default;
    Value -> Value
  end.

%% @doc Retrieves a configuration key from the application environment.
-spec cfg_get(atom()) -> term().
cfg_get(Key) ->
  case application:get_env(erlastica, Key) of
    undefined -> undefined;
    {ok, Val} -> Val
  end.

%% @doc Creates the given index with the given json parameters.
-spec create_index(index(), ejson()) -> result().
create_index(IndexName, Options) ->
  req(IndexName, put, Options, [], [], [], [200]).

%% @doc Deletes the given index.
-spec delete_index(index()) -> result().
delete_index(IndexName) ->
  req(IndexName, delete, {[]}, [], [], [], [404, 200]).

%% @doc Indexes the given document with the given type.
-spec index(index(), type(), doc()) -> result().
index(IndexName, Type, Doc) ->
  index(IndexName, Type, Doc, []).

%% @doc Indexes the given document with the given type and options.
-spec index(index(), type(), doc(), [index_option()]) -> result().
index(IndexName, Type, Doc, Options) ->
  Id = proplists:get_value(id, Options, []),
  QueryString = case proplists:get_value(version, Options) of
    undefined -> [];
    Version -> [
      {"version_type", "external"},
      {"version", Version}
    ]
  end,
  req(IndexName, post, Doc, [Type] ++ [Id], [], QueryString, [200, 201, 204]).

%% @doc Does a scroll&scan, as documented here:
%% http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-request-scroll.html#scroll-scan
%% CursorTime is something like "30s", "1m", etc.
%% The callback function will be called with every set of results as the scroll
%% goes on.
-spec scan(index(), querydoc(), string(), function()) -> ok.
scan(IndexName, Query, CursorTime, Fun) ->
  scan(IndexName, "", Query, CursorTime, Fun).

-spec scan(index(), type(), querydoc(), string(), function()) -> ok.
scan(IndexName, DocType, Query, CursorTime, Fun) ->
  Path = case DocType of
    "" -> ["_search"];
    DocType -> [DocType, "_search"]
  end,
  {ok, 200, _Headers, {Body}} = req(
    IndexName, get, Query, Path, [],
    [{"scroll", CursorTime}, {"search_type", "scan"}], [200]
  ),
  ScrollId = proplists:get_value(<<"_scroll_id">>, Body),
  Total = case proplists:get_value(<<"hits">>, Body) of
    undefined -> 0;
    {HitsBody} -> proplists:get_value(<<"total">>, HitsBody)
  end,
  scroll(IndexName, CursorTime, binary_to_list(ScrollId), 0, Total, Fun).

-spec find(index(), querydoc()) -> ejson().
find(IndexName, Query) ->
  {ok, 200, _Headers, Body} = req(
    IndexName, get, Query, ["_search"], [], [], [200]
  ),
  Body.

-spec find_by_id(index(), type(), string()) -> ejson().
find_by_id(IndexName, Type, Id) ->
  {ok, 200, _Headers, Body} = req(
    IndexName, get, {[]}, [Type, Id], [], [], [200]
  ),
  Body.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Given an index name, will return true index name and elasticsearch
%% host/port.
-spec index_host_config(index()) -> {string(), string(), pos_integer()}.
index_host_config(Index) ->
  IndexConfig = proplists:get_value(Index, cfg_get(indices)),
  IndexName = proplists:get_value(index, IndexConfig),
  HostMapping = proplists:get_value(host, IndexConfig),
  HostConfig = proplists:get_value(HostMapping, cfg_get(hosts)),
  {
    IndexName,
    proplists:get_value(host, HostConfig),
    proplists:get_value(port, HostConfig)
  }.

-spec scroll(
  index(), string(), string(),
  non_neg_integer(), non_neg_integer(), function()
) -> ok.
scroll(_IndexName, _CursorTime, _ScrollId, Total, Total, _Fun) ->
  ok;

scroll(IndexName, CursorTime, ScrollId, Current, Total, Fun) ->
  lager:debug("Scrolling: ~p / ~p", [Current, Total]),
  {ok, 200, _Headers, {Body}} = req(
    IndexName, get, {[]}, ["_search", "scroll"], [], [
      {"scroll", CursorTime},
      {"search_type", "scan"},
      {"scroll_id", ScrollId}
    ], [200]
  ),
  NewScrollId = proplists:get_value(<<"_scroll_id">>, Body),
  Hits = case proplists:get_value(<<"hits">>, Body) of
    undefined -> 0;
    {HitsBody} -> proplists:get_value(<<"hits">>, HitsBody)
  end,
  Fun(Hits, Total),
  scroll(
    IndexName, CursorTime, binary_to_list(NewScrollId),
    Current + length(Hits), Total, Fun
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec req(
  index(), egetter:method(),
  egetter:body(), [string()], [egetter:option()], egetter:query_string(),
  [egetter:status()]
) -> result().
req(
  IndexName, Method, Body, PathComponents, Options, QueryString, OkStatuses
) ->
  {Index, Host, Port} = index_host_config(IndexName),
  Path = case PathComponents -- ["scroll"] of
    PathComponents -> [Index|PathComponents];
    _ -> PathComponents
  end,
  {_, RStatus, RHeaders, RBody} = egetter:req(Options ++ [
    {host, Host},
    {port, Port},
    {scheme, "http"},
    {method, Method},
    {body, jiffy:encode(Body)},
    {ibrowse_options, cfg_get(ibrowse_options, [])},
    {use_proxy, false},
    {follow_redirect, false},
    {path_components, Path},
    {query_string, QueryString}
  ]),
  Json = jiffy:decode(RBody),
  IsOk = lists:member(RStatus, OkStatuses),
  if IsOk -> {ok, RStatus, RHeaders, Json};
    true -> {error, RStatus, RHeaders, Json}
  end.
