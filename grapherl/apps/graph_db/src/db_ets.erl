-module(db_ets).
-behaviour(gen_db).

-export([init_db/2,
         delete_db/2,
         read_all/2,
         insert/3,
         insert_many/3,
         delete_all/2]).

-include_lib("graph_db_records.hrl").

%% ----------------------------------------------------------------------------
%% Database API
%% ----------------------------------------------------------------------------

%% init db
init_db(MetricName, State) ->
    ets:new(MetricName, [set, public, named_table,
                         {write_concurrency, true},
                         {read_concurreny, false}]),
    {ok, State}.


%% remove Database
delete_db(MetricName, State) ->
    true = ets:delete(MetricName),
    {ok, State}.


%% read all data points from db
read_all(MetricName, State) ->
    MetricPoints     = ets:match(MetricName, '$1'),
    {ok, DataPoints} = unwrap_points(MetricPoints, []),
    {ok, DataPoints, State}.


%% insert data point into db
insert(MetricName, DataPoint, State) ->
    true = ets:insert(MetricName, DataPoint),
    {ok, State}.


%% insert multiple data points
insert_many(_MetricName, [], State) ->
    {ok, State};
insert_many(MetricName, [DataPoint | Rest], State) ->
    insert(MetricName, DataPoint, State),
    insert_many(MetricName, Rest, State).


%% delete all data points
delete_all(MetricName, State) ->
    true = ets:delete_all_objects(MetricName),
    {ok, State}.



%% ----------------------------------------------------------------------------
%% Internal Functions
%% ----------------------------------------------------------------------------

unwrap_points([], Acc) ->
    {ok, lists:reverse(Acc)};
unwrap_points([[DataPoint] | Rest], Acc) ->
    unwrap_points(Rest, [DataPoint | Acc]).
