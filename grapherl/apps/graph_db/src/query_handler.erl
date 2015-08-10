-module(query_handler).

-behaviour(gen_server).

%% API functions
-export([start_link/0
        ,get_data/2
        ,get_metric_list/0
        ,get_metric_data/4
        ,load_data/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("../include/graph_db_records.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================
get_data(From, Query) ->
    gen_server:cast(?MODULE, {get_data, From, Query}).

%% Collect metric data from all nodes
get_metric_list() ->
    {ok, Maps} = db_manager:get_metric_maps(),
    List = lists:map(
             fun({{MetricName, _}, MetricData}) ->
                     {ok, [DbFd, CacheFd]} =
                         graph_utils:get_args(MetricData, [{db_fd, live},
                                                           cache_fd]),
                     {ok, Clients0} = db_worker:get_clients(db, DbFd),
                     {ok, Clients1} = db_worker:get_clients(cache, CacheFd),
                     Clients = lists:usort(lists:flatten([Clients0, Clients1])),
                     {MetricName, Clients}
             end, Maps),
    {ok, List}.

%% retrieve metric data
get_metric_data(Metric, Client, {Start, End}, Granularity) ->
    {ok, Granularity0} = db_utils:process_granularity(Granularity),
    {ok, Data0} = gen_server:call(?MODULE, {get_data, {Metric, Client, Start, End, 
                                                      Granularity0}}),
    {ok, Type}  = db_manager:get_metric_type(Metric),
    {ok, Data}  = process_data(lists:keysort(1, Data0), Granularity, Type),
    %% io:format("~n~n sending replay ~n~p~n", [lists:keysort(1, Data)]),
    {ok, lists:keysort(1, Data)}.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ets:new(?MODULE, [set, public, named_table
                     ,{write_concurrency, false}
                     ,{read_concurrency, true}]),
    {ok, #{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get_data, Query}, _From, State) ->
    %% for optimization we can cache query.
    io:format("getting data ~p~n", [Query]),
    {ok, Data} = load_data(Query),

    {reply, {ok, Data}, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
%% not used
handle_cast({get_data, From, Query}, State) ->
    {ok, Data} = load_data(Query),
    From ! {ok, Data},
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%%============================================================================
%%% Internal functions
%%%============================================================================
%% load data tries to return queried range at the specified is available.
%% If the range has been purged then it returns the data and lower granulrity.
%% If the range has not yet been compressed or has been partially compressed
%% then it return range with data points at different granularity.
load_data({Mn, Cn, StartR, EndR, Granularity}) ->
    {ok, MetricMap} = db_manager:load_metric_map({Mn, Cn}),

    case process_range({Mn, Cn, Granularity}, {StartR, EndR}) of

        %% if the range was purged then move to lower ganularity
        {ok, true} ->
            case db_utils:get_next_type(Granularity) of
                stop -> load_data(MetricMap, {Mn, Cn, StartR, EndR, Granularity}, []);
                _ -> load_data({Mn, Cn, StartR, EndR, db_utils:get_next_type(Granularity)})
            end;

        {ok, false} ->

            %% check if the given range was compressed to Granularity
            case db_daemon:was_compressed({Mn, Cn, Granularity}, {StartR, EndR}) of

                %% if data has not been compressed to current granularity then
                %% move to a higher granularity
                {ok, false} ->
                    case db_utils:get_prev_type(Granularity) of
                        stop ->
                            load_data(MetricMap, {Mn, Cn, StartR, EndR, Granularity}, []);
                        _ ->
                            load_data({Mn, Cn, StartR, EndR, db_utils:get_prev_type(Granularity)})
                    end;

                %% if the data was compressed to the current granularity then
                %% start looking for the data with current granularity
                {ok, true} ->
                    load_data(MetricMap, {Mn, Cn, StartR, EndR, Granularity}, []);

                %% if data is partially compressed, then part of the data is at
                %% higher granularity. So, start looking for the data from higher
                %% granularity
                {ok, partial} ->
                    load_data(MetricMap, {Mn, Cn, StartR, EndR,
                                          db_utils:get_prev_type(Granularity)}, [])
            end;

        %% if the range was partially compressed then move to higher granularity
        %% and start collecting data
        {ok, partial} ->
            load_data(MetricMap, {Mn, Cn, StartR, EndR, Granularity}, [])
    end.


%% check the given range has been purged
process_range({Mn, Cn, Granularity}, {_StartR, _} = Range) ->
    db_daemon:was_purged({Mn, Cn, db_utils:get_next_type(Granularity)}, Range).
    %Diff = erlang:abs(StartR - db_utils:unix_time()),
    %AggrSize = db_utils:get_aggregation_size(Granularity),
    %% if
    %%     %% if the StartR is greater than the data must definitely have been
    %%     %% compressed. It is possible that daemon might be purging the data
    %%     %% so it we should add little time to relax the condition
    %%     Diff > AggrSize + 3600 ->
    %%         {true, purged};
    %%     true ->
    %%         %% was the current range purged to next level of Granularity
    %%         db_daemon:was_purged({Mn, Cn, db_utils:get_next_type(Granularity)}, Range)
    %% end.



%% TODO send data when collected rather sending all of the data at once

%% get data for specified range started from a given Granularity and keep moving
%% to lower ganularity
load_data(_, {_Mn, _Cn, _StartR, _EndR, stop}, Acc) ->
    {ok, Acc};

load_data(MetricMap, {Mn, Cn, StartR, EndR, ?SEC}, Acc) ->
    {cache_fd, CacheFd} = lists:keyfind(cache_fd, 1, MetricMap),

    Type            = db_utils:get_next_type(?SEC),
    {ok, DataCache} = db_worker:read_cache({Cn, CacheFd, {StartR, EndR}}),
    AccNew0         = lists:keysort(1, lists:flatten([DataCache, Acc])),
    case reamining_range(AccNew0, {StartR, EndR}) of
        {ok, none} ->
            %load_data(MetricMap, {Mn, Cn, StartR, EndR, Type}, [Data | Acc]);
            {ok, AccNew0};

        {ok, {StartRR, EndRR}} ->
            {_, DbFd}    = lists:keyfind({db_fd, live}, 1, MetricMap),
            {ok, DataDb} = db_worker:read_db({Cn, DbFd, {StartRR, EndRR}}),
            AccNew       = lists:keysort(1, lists:flatten([DataDb, AccNew0])),

            case reamining_range(AccNew, {StartRR, EndRR}) of
                {ok, none} ->
                    %load_data(MetricMap, {Mn, Cn, StartRR, EndRR, Type}, [DataDb | Acc0]);
                    {ok, AccNew};

                {ok, {StartRRR, EndRRR}} ->
                    load_data(MetricMap, {Mn, Cn, StartRRR, EndRRR, Type}, AccNew)
            end

    end;

load_data(MetricMap, {Mn, Cn, StartR, EndR, Granularity}, Acc) ->
    Type = db_utils:get_next_type(Granularity),

    case lists:keyfind({db_fd, Granularity}, 1, MetricMap) of
        false        ->
            load_data(MetricMap, {Mn, Cn, StartR, EndR, Type}, Acc);

        {_Key, DbFd} ->
            {ok, Data} = db_worker:read_db({Cn, DbFd, {StartR, EndR}}),
            AccNew     = lists:keysort(1, lists:flatten([Data, Acc])),
            case reamining_range(AccNew, {StartR, EndR}) of
                {ok, none}  -> {ok, AccNew};
                {ok, {StartRR, EndRR}} ->
                    load_data(MetricMap, {Mn, Cn, StartRR, EndRR, Type}, AccNew)
            end
    end.


%% check if the fetched data completes the required range
reamining_range([], Range) ->
    {ok, Range};
reamining_range(Data, {_StartR, EndR}) ->
    %Data0      = lists:keysort(1, Data),
    %{Start, _} = erlang:hd(lists:reverse(Data0)),
    {End, _}   = erlang:hd(Data),

    %% probably very optimistic that this condition is here
    if
        %Start < StartR orelse End > EndR ->
        End > EndR ->
            {ok, {End, EndR}};
        true -> {ok, none}
    end.




%% ============================================================================
%% compress data to match it to queried granularity
%% ============================================================================
process_data(Data, Granularity, Type) ->
    {ok, Data0} = case Granularity of
                      <<"sec">> -> {ok, Data};
                      _ -> compress_data(Data, Granularity, Type)
                  end,
    if
        erlang:length(Data0) > 1000 ->
            {ok, NewG}  = db_utils:lower_query_granularity(Granularity),
            {ok, Data1} = recompress_data(Data0, NewG, Type, 1000),
            {ok, lists:ukeysort(1, Data1)};
        true ->
            {ok, lists:ukeysort(1, Data0)}
    end.
        

%% recompress data to bring the data size below the threshold
recompress_data(Data, _G, _T, Limit) when erlang:length(Data) =< Limit ->
    {ok, Data};
recompress_data(Data, Granularity, Type, Limit) ->
    {ok, DataC} = compress_data(Data, Granularity, Type),
    {ok, NewG} = db_utils:lower_query_granularity(Granularity),
    recompress_data(DataC, NewG, Type, Limit).


%% compress data at given granularity
compress_data([], _Granularity, _Type) ->
    {ok, []};
compress_data([{Key, Val} | Rest] = _Data, Granularity, Type) ->
    {ok, Interval} = db_utils:query_granularity_to_interval(Granularity),
    IntK           = erlang:binary_to_integer(Key),
    TAcc           = [{IntK, Val}],
    compress_data0(Rest, Granularity, Type, Interval, TAcc, []).



compress_data0([], _, _Type, Interval, [], Acc) when Interval > 0 ->
    {ok, lists:ukeysort(1, Acc)};

compress_data0([], _, Type, Interval, TAcc, Acc) when Interval > 0 ->
    %% compress the remaining points anyway
    {ok, NewKey, NewVal} = compress_acc(TAcc, Type),
    {ok, lists:ukeysort(1, [{NewKey, NewVal} | Acc])};

%% in case we have Data at the required granularity then Interval =< 0 for every next
%% point. So in that case we directly add then to Acc
%% compress_data0([{NKey, NVal} | Rest], Granularity, Type, Interval, [{Key,Val}], Acc)
%%   when Interval =< 0 ->
%%     TAcc             = [{binary_to_integer(NKey), NVal}],
%%     {ok, IntervalN}  = db_utils:query_granularity_to_interval(Granularity),
%%     compress_data0(Rest, Granularity, Type, IntervalN, TAcc,
%%                    [{integer_to_binary(Key), Val} | Acc]);

compress_data0(Rest, Granularity, Type, Interval, TAcc, Acc) when Interval =< 0 ->
    {ok, NewKey, NewVal} = compress_acc(TAcc, Type),
    {ok, IntervalNew}    = db_utils:query_granularity_to_interval(Granularity),
    case Rest of
        [] ->
            compress_data0(Rest, Granularity, Type, IntervalNew, [],
                           [{NewKey, NewVal} | Acc]);
        [{Key, Val} | Rest0] ->
            IntK = erlang:binary_to_integer(Key),
            compress_data0(Rest0, Granularity, Type, IntervalNew,
                           [{IntK, Val}], [{NewKey, NewVal} | Acc])
    end;
compress_data0([{Key, Val} | Rest], Granularity, Type, Interval,
               [{PrevK, _} | _AccR] = TAcc, Acc) when Interval > 0 ->
    %% subtract interval
    IntK       = erlang:binary_to_integer(Key),
    Interval0  = IntK - PrevK,
    Diff       = erlang:abs(Interval - Interval0),
    compress_data0(Rest, Granularity, Type, Diff,
                   [ {IntK, Val} | TAcc ], Acc).



compress_acc(Acc, Type) ->
    Values     = [Val || {_Key, Val} <- Acc],
    Keys       = [Key || {Key, _Val} <- Acc],
    {Mod, Fun} = db_utils:get_merge_fun(Type),
    %% taken from db_daemon
    NewVal     = erlang:apply(Mod, Fun, [Values]),
    AvgKey     = erlang:integer_to_binary(
                   erlang:round(
                     graph_utils:binary_to_realNumber(
                       graph_utils:mean(Keys)))),
    {ok, AvgKey, NewVal}.
