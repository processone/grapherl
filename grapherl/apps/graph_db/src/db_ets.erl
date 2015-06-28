-module(db_ets).
-behaviour(gen_db).

-export([init_db/2
        ,open_db/2
        ,close_db/1
        ,delete_db/1
        ,read_all/1
        ,insert/2
        ,insert_many/2
        ,clear_db/1
        ]).

-include_lib("graph_db_records.hrl").

%% ----------------------------------------------------------------------------
%% Database API
%% ----------------------------------------------------------------------------
%% NOTE: the State var is only sotre attributes related to db like url etc. its
%% not update at all.

%% init db: return {ok, Params}
%% any db mod that is going to do ram storage should return {ok, self(), Fd}
init_db(MetricName, Args) when is_binary(MetricName) ->
    init_db(binary_to_atom(MetricName, utf8), Args);

init_db(MetricName, _Args) when is_atom(MetricName) ->
    ets:new(MetricName, [set, public, named_table,
                         ordered_set,
                         {write_concurrency, false},
                         {read_concurrency, false}]),
    {ok, self(), MetricName}.


open_db(MetricName, Args) when is_binary(MetricName) ->
    open_db(binary_to_atom(MetricName, utf8), Args);
open_db(MetricName, Args) when is_atom(MetricName) ->
    case ets:info(MetricName) of
        undefined -> init_db(MetricName, Args);
        _         -> {ok, self(), MetricName}
    end.

close_db(MetricName) when is_atom(MetricName) ->
    delete_db(MetricName).

%% remove Database
delete_db(MetricName) when is_atom(MetricName) ->
    true = ets:delete(MetricName),
    {ok, success}.


%% read all data points from db
read_all(MetricName) when is_atom(MetricName) ->
    MetricPoints     = ets:match(MetricName, '$1'),
    {ok, DataPoints} = unwrap_points(MetricPoints, []),
    {ok, DataPoints}.


%% insert data point into db
insert(MetricName, DataPoint) when is_atom(MetricName) ->
    true = ets:insert(MetricName, DataPoint),
    {ok, success}.


%% insert multiple data points
insert_many(_MetricName, []) ->
    {ok, success};
insert_many(MetricName, [DataPoint | Rest]) ->
    insert(MetricName, DataPoint),
    insert_many(MetricName, Rest).


%% delete all data points
clear_db(MetricName) when is_atom(MetricName) ->
    true = ets:delete_all_objects(MetricName),
    {ok, success}.


%% ----------------------------------------------------------------------------
%% Internal Functions
%% ----------------------------------------------------------------------------

unwrap_points([], Acc) ->
    {ok, lists:reverse(Acc)};
unwrap_points([[DataPoint] | Rest], Acc) ->
    unwrap_points(Rest, [DataPoint | Acc]).
