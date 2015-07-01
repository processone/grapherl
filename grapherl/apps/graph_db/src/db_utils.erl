-module(db_utils).

-export([gc/0
        ,unix_time/0
        ,get_msg_queue_name/1
        ,db_live/1
        ,get_avg_interval/1
        ,db_minutes/1
        ,db_hours/1
        ,db_days/1
        ,to_metric_name/1
        ,get_metric_name/2
        ,get_next_type/1
        ,get_interval/1
        ,get_aggregation_size/1
        ]).

-include_lib("../include/graph_db_records.hrl").

%% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
-define(UNIX_EPOCH, 62167219200).


%%%===================================================================
%%% API
%%%===================================================================

%% garbage collect if the memory consumption exceeds 10Mb
gc() ->
    case erlang:process_info(self(), memory) of
        {memory, Mem} when Mem > 10*1024*1024 ->
            erlang:garbage_collect(self());
        _ ->
            ok
    end.
    

unix_time() ->
    datetime_to_unix_time(erlang:universaltime()).


%% generate name for message queue based on port
get_msg_queue_name(Port) ->
    erlang:list_to_atom("queue" ++ erlang:integer_to_list(Port)).


%% epoch time
datetime_to_unix_time({{_,_,_},{_,_,_}} = DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - ?UNIX_EPOCH.


%% names for db objects based on granularity
db_live(Name) when is_binary(Name) ->
    Name.

db_minutes(Name) when is_binary(Name) ->
    <<Name/binary, "_minutes">>.

db_hours(Name) when is_binary(Name) ->
    <<Name/binary, "_hours">>.

db_days(Name) when is_binary(Name) ->
    <<Name/binary, "_days">>.


%% convert to metric name for metric id
to_metric_name({Mid, Cn}) ->
    <<Mid/binary, "-", Cn/binary>>.


%% for a list for {Key, Val} get the average difference between keys
get_avg_interval([]) ->
    error;
get_avg_interval([{_K, _V}]) ->
    error;
get_avg_interval(List) ->
    get_avg_interval(List, 0, 0).

get_avg_interval([], Count, Acc) ->
    Acc/Count;
get_avg_interval([{_K1, _}], Count, Acc) ->
    Acc/Count;
get_avg_interval([{K1, _}, {K2, _} | Rest], Count, Acc) ->
    AccNew = erlang:abs(binary_to_integer(K2) - binary_to_integer(K1)) + Acc,
    get_avg_interval(Rest, Count + 1, AccNew).


%% gives the name of next metric based on current type.
get_metric_name(live, Name) ->
    db_live(Name);
get_metric_name(?SEC, Name) ->
    %% we are not expecting granularity lower than second so it would mean
    %% that we want the db storing the live data.
    db_live(Name);
get_metric_name(?MIN, Name) ->
    db_minutes(Name);
get_metric_name(?HOUR, Name) ->
    db_hours(Name);
get_metric_name(?DAY, Name) ->
    db_days(Name);
get_metric_name(_, Name) ->
    db_days(Name).


get_next_type(?SEC)  -> min;
get_next_type(?MIN)  -> hour;
get_next_type(?HOUR) -> day;
get_next_type(?DAY)  -> stop;
get_next_type(_)     -> stop.

get_interval(?SEC)  -> 60;
get_interval(?MIN)  -> 3600;
get_interval(?HOUR) -> 86400;
get_interval(?DAY)  -> 86400;
get_interval(_)     -> 86400.

get_aggregation_size(?SEC)  -> 3600 * 24;
get_aggregation_size(?MIN)  -> 3600 * 24 * 7;
get_aggregation_size(?HOUR) -> 3600 * 24 * 365;
%% keep the diff for day to be large so that no compression occurs
get_aggregation_size(?DAY)  -> 3600 * 24 * 365 * 100;
get_aggregation_size(_)     -> 3600 * 24 * 365 * 100.


