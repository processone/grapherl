-module(db_ets).
-behaviour(gen_db).

-export([init_db/2
        ,open_db/2
        ,close_db/1
        ,delete_db/1
        ,read/2
        ,read_all/1
        ,insert/3
        ,insert_many/3
        ,delete_many/3
        ,clear_client/2
        ,get_range/2
        ,get_clients/1
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
    ets:new(MetricName, [public, named_table,
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


% read all points from given client.
read(MetricName, Cn) when is_atom(MetricName) ->
    MetricPoints     = ets:match(MetricName, {{Cn, '$1'}, '$2'}),
    {ok, DataPoints} = unwrap_points(MetricPoints, []),
    {ok, DataPoints}.


%% read all data points from metric cache
%% returns [{Client1, [{k1,v1}, {k2,v2}, ...]}, {Client2, [{k1,v1}, ...]}, ...]
read_all(MetricName) when is_atom(MetricName) ->
    MetricPoints     = ets:match(MetricName, {'$1', '$2'}),
    {ok, DataPoints} = group_points(MetricPoints, []),
    {ok, DataPoints}.


%% insert data point into db
insert(MetricName, Client, {Key, Val}) when is_atom(MetricName) ->
    true = ets_insert(MetricName, {{Client, Key}, Val}),
    {ok, success}.


%% insert multiple data points
%% DataPoints: [{K1,V1}, {K2,V2} ...]
insert_many(MetricName, Client, DataPoints) ->
    {ok, Points} = insert_many0(Client, DataPoints, []),
    true         = ets_insert(MetricName, Points),
    {ok, success}.

insert_many0(_Client, [], Acc) ->
    {ok, lists:reverse(Acc)};
insert_many0(Client, [{Key,Val} | Rest], Acc) ->
    insert_many(Client, Rest, [{{Client, Key}, Val} | Acc]).


% TODO implement this (just to complete the behaviours)
delete_many(_MetricName, _Client, _Points) ->
    {ok, success}.

%% delete all data points for a client
clear_client(MetricName, Client) when is_atom(MetricName) ->
    true = ets:match_delete(MetricName, {{Client, '$1'}, '$2'}),
    {ok, success}.


get_range(MetricName, {Cn, Start, End}) ->
    Data = ets:foldl(
             fun(Element, Acc) ->
                     case Element of
                         {{Cn, Key}, Val} when Key =< Start andalso Key >= End ->
                             [{Key, Val} | Acc];
                         _ ->
                             Acc
                     end
             end, [], MetricName),
    {ok, Data}.


%% return all unique clients
get_clients(MetricName) ->
    Clients = lists:usort(lists:flatten(
                            ets:match(MetricName, {{'$1', '_'}, '_'}))),
    {ok, Clients}.

%% ----------------------------------------------------------------------------
%% Internal Functions
%% ----------------------------------------------------------------------------
group_points([], Acc) ->
    {ok, Acc};
group_points([[{Cn, Key}, Val] | Rest], Acc) ->
    case lists:keyfind(Cn, 1, Acc) of
        false ->
            group_points(Rest, [{Cn, [{Key, Val}]} | Acc]);
        {Cn, Data} ->
            group_points(Rest, lists:keystore(Cn, 1, Acc,
                                              {Cn, lists:reverse([{Key, Val} | Data])}))
    end.


ets_insert(Tab, Points) ->
    ets:insert(Tab, Points).


unwrap_points([], Acc) ->
    {ok, lists:reverse(Acc)};
unwrap_points([[K,V] | Rest], Acc) ->
    unwrap_points(Rest, [{K,V} | Acc]).
