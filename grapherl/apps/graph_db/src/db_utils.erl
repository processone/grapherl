-module(db_utils).

-export([gc/0
        ,get_merge_fun/1
        ,unix_time/0
        ,get_msg_queue_name/1
        ,db_live/1
        ,get_avg_interval/1
        ,db_minutes/1
        ,db_hours/1
        ,db_days/1
        ,to_metric_name/1
        ,get_metric_name/2
        ,process_granularity/1
        ,lower_query_granularity/1
        ,query_granularity_to_interval/1
        ,get_prev_type/1
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
    

get_merge_fun(<<"g">>) ->
    {graph_utils, gauge};
get_merge_fun(<<"c">>) ->
    {graph_utils, counter};
get_merge_fun(_) ->
    {graph_utils, mean}.



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
to_metric_name({Mid, _Cn}) ->
    <<Mid/binary, "-metric">>.
    %%<<Mid/binary, "-", Cn/binary>>.


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


process_granularity(<<"sec">>) ->
    {ok, ?SEC};
process_granularity(<<"min">>) ->
    {ok, ?MIN};
process_granularity(<<"hour">>) ->
    {ok, ?HOUR};
process_granularity(<<"day">>) ->
    {ok, ?DAY};
process_granularity(_) ->
    {ok, ?DAY}.


lower_query_granularity(<<"sec">>) ->
    {ok, <<"min">>};
lower_query_granularity(<<"min">>) ->
    {ok, <<"hour">>};
lower_query_granularity(<<"hour">>) ->
    {ok, <<"day">>};
lower_query_granularity(<<"day">>) ->
    {ok, <<"week">>};
lower_query_granularity(<<"week">>) ->
    {ok, <<"month">>};    
lower_query_granularity(<<"year">>) ->
    {ok, <<"year">>};
lower_query_granularity(_) ->
    {ok, <<"year">>}.



query_granularity_to_interval(<<"sec">>) ->
    {ok, 30};
query_granularity_to_interval(<<"min">>) ->
    {ok, get_interval(?SEC)};
query_granularity_to_interval(<<"hour">>) ->
    {ok, get_interval(?MIN)};
query_granularity_to_interval(<<"day">>) ->
    {ok, get_interval(?HOUR)};
query_granularity_to_interval(<<"week">>) ->
    {ok, get_interval(?DAY) * 7};
query_granularity_to_interval(<<"month">>) ->
    {ok, get_interval(?DAY) * 30};
query_granularity_to_interval(<<"year">>) ->
    {ok, get_interval(?DAY) * 365};
query_granularity_to_interval(_) ->
    {ok, get_interval(?DAY) * 365}.
    %{ok, ?DAY}.


get_prev_type(?SEC)  -> stop;
get_prev_type(?MIN)  -> ?SEC;
get_prev_type(?HOUR) -> ?MIN;
get_prev_type(?DAY)  -> ?HOUR;
get_prev_type(_)     -> ?DAY.


get_next_type(?SEC)  -> ?MIN;
get_next_type(?MIN)  -> ?HOUR;
get_next_type(?HOUR) -> ?DAY;
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


