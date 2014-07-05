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
  {_, RStatus, RHeaders, RBody} = egetter:req(Options ++ [
    {host, Host},
    {port, Port},
    {scheme, "http"},
    {method, Method},
    {body, jiffy:encode(Body)},
    {ibrowse_options, cfg_get(ibrowse_options, [])},
    {use_proxy, false},
    {follow_redirect, false},
    {path_components, [Index|PathComponents]},
    {query_string, QueryString}
  ]),
  Json = jiffy:decode(RBody),
  IsOk = lists:member(RStatus, OkStatuses),
  if IsOk -> {ok, RStatus, RHeaders, Json};
    true -> {error, RStatus, RHeaders, Json}
  end.
