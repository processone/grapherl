-module(db_levelDB).
-behaviour(gen_db).

-export([init_db/2
        ,open_db/2
        ,close_db/1
        ,delete_db/1
        ,read_all/1
        ,insert/2
        ,insert_many/2
        ,delete_many/2
        ,clear_db/1]).

-include_lib("graph_db_records.hrl").

%% ----------------------------------------------------------------------------
%% Database API
%% ----------------------------------------------------------------------------

%% this func will be called whenever the app wants a handler for the db object.
%% so it is up to the this module to detect and handle if the db already exits.

%% init db: return {ok, Params}
%% make to store the data as ordered set.
init_db(MetricName, Args) when is_binary(MetricName) ->
    {ok, [Dir]} = graph_utils:get_args(Args, [storage_dir]),
    MetricPath  = binary_to_list(<< Dir/binary, MetricName/binary>> ),
    {ok, Ref}   = eleveldb:open(MetricPath, [{create_if_missing, true}]),
    {ok, #{ref => Ref, dir => MetricPath}}.

%% should try to open the existing db and create it if not already present
open_db(MetricName, Args) when is_binary(MetricName) ->
    init_db(MetricName, Args).

close_db(#{ref := Ref}) ->
    ok = eleveldb:close(Ref),
    {ok, success}.

%% remove Database
delete_db(#{dir := MetricPath} = _MetricRef) ->
    ok = eleveldb:destroy(MetricPath, []),
    {ok, success}.


%% read all data points from db
read_all(#{ref := Ref} = _MetricRef) ->
    Data = lists:reverse(eleveldb:fold(Ref, fun({K, V}, Acc) -> [{K, V} | Acc] end, [], [])),
    {ok, Data}.


%% insert data point into db
insert(#{ref := Ref}, {Key, Value}) ->
    ok = eleveldb:put(Ref, Key, Value, []),
    {ok, success}.


%% insert multiple data points
insert_many(#{ref := Ref}, Points) ->
    ok = eleveldb:write(Ref, [{put, K, V} || {K,V} <- Points], []),
    {ok, success}.

%% insert multiple data points
delete_many(#{ref := Ref}, Points) ->
    ok = eleveldb:write(Ref, [{delete, K} || {K, _V} <- Points], []),
    {ok, success}.

%% delete all data points
clear_db(#{ref := Ref}) ->
    Operation = eleveldb:fold_keys(Ref,
                                   fun(K, Acc) ->
                                           [{delete, K} | Acc]
                                   end, [], []),
    ok = eleveldb:write(Ref, Operation, []),
    {ok, success}.


%% ----------------------------------------------------------------------------
%% Internal Functions
%% ----------------------------------------------------------------------------

