-module(db_levelDB).
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
read(#{ref := Ref} = _MetricRef, Client) ->
    Data = lists:reverse(eleveldb:fold(Ref,
                                       fun({Term, V}, Acc) ->
                                               case binary:split(Term, [<<",">>]) of
                                                   [Client, K] -> [{K, V} | Acc];
                                                   _           -> Acc
                                               end
                                       end, [], [])),
    {ok, Data}.


%% TODO implement this (but this is not used anywhere just to be complient
%% the api)
read_all(_) ->
    ok.


%% insert data point from a given client into db 
insert(#{ref := Ref}, Client, {Key, Value}) ->
    ok = eleveldb:put(Ref, construct_key(Client, Key), Value, []),
    {ok, success}.


%% insert multiple data points from client into db
insert_many(#{ref := Ref}, Client, Points) ->
    ok = eleveldb:write(Ref, [{put, construct_key(Client, K), V} 
                              || {K,V} <- Points], []),
    {ok, success}.


%% delete multiple data points for a client
delete_many(#{ref := Ref}, Client, Points) ->
    ok = eleveldb:write(Ref, [{delete, construct_key(Client, K)} ||
                                 {K, _V} <- Points], []),
    {ok, success}.


%% delete all data points for a given client
clear_client(#{ref := Ref}, Client) ->
    Operation = eleveldb:fold_keys(Ref,
                                   fun(K, Acc) ->
                                           [{delete, construct_key(Client, K)} | Acc]
                                   end, [], []),
    ok = eleveldb:write(Ref, Operation, []),
    {ok, success}.


get_range(#{ref := Ref}, {Client, Start, End}) ->
    Data = lists:reverse(
             eleveldb:fold(Ref,
                           fun({Term, V}, Acc) ->
                                   case binary:split(Term, [<<",">>]) of
                                       [Client, K] when K =< Start andalso
                                                        K >= End ->
                                           [{K, V} | Acc];
                                       _ ->
                                           Acc
                                   end
                           end, [], [])),
    {ok, Data}.


%% returns unique clients
get_clients(#{ref := Ref}) ->
    Clients = lists:usort(
                eleveldb:fold(Ref,
                              fun({Term, _V}, Acc) ->
                                      case binary:split(Term, [<<",">>]) of
                                          [Client, _K] ->
                                              [Client | Acc];
                                          _  ->
                                              Acc
                                      end
                              end, [], [])),
    {ok, Clients}.


%% ----------------------------------------------------------------------------
%% Internal Functions
%% ----------------------------------------------------------------------------
construct_key(Client, Key) ->
    <<Client/binary, ",", Key/binary>>.
