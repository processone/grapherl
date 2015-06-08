-module(db_daemon).

-behaviour(gen_server).

%% API functions
-export([start_link/0
        ,aggregate_data/4]).

%% test exports
-export([get_granularity/1
        ,generate_data/2
        ,merge_points/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(TIMEOUT, 5000).

%%%===================================================================
%%% API functions
%%%===================================================================

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
    {ok, Timeout} = application:get_env(graph_db, db_daemon_timeout),
    {ok, DbDir}   = application:get_env(graph_db, storage_dir),
    {ok, #{timeout => Timeout, storage_dir => DbDir}, 0}.

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
handle_info(timeout,  State) ->
    #{storage_dir := DbDir, timeout := Timeout, db_mod := DbMod} = State,
    case db_manager:get_metric_maps() of
        {ok, MapList} ->
            purge_data(DbMod, DbDir, MapList),
            {noreply, State, Timeout};
        _ ->
            {noreply, State, ?TIMEOUT}
    end;

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

%%%===================================================================
%%% Internal functions
%%%===================================================================

purge_data(_DbMod, _DbDir, []) ->
    ok;
purge_data(DbMod, DbDir, [{_K, MetricData} | Rest]) ->
    %% Name is the live name for the Metric
    {name, Name} = lists:keyfind(name, 1, MetricData),
    {db_fd, DbFd} = lists:keyfind(db_fd, 1, MetricData),

    spawn(?MODULE, aggregate_data, [DbDir, DbMod, Name, DbFd]),
    purge_data(DbMod, DbDir, Rest).


aggregate_data(DbDir, DbMod, BaseName, State) ->
    %% optimize granularity calculation
    {ok, KeyVal} = DbMod:read_all(State),
    case get_granularity(KeyVal) of
        {stop, _Step}  ->
            ok;
        {Type,Step}  ->            
            %MergeFun  = fun(List) -> graph_utils:mean(List) end,
            MergeFun  = {graph_utils, mean},
            {NewPoints, DelPoints} = merge_points(KeyVal, {Type, Step}, MergeFun),

            NextMetric    = db_utils:get_metric_name(db_utils:get_next_step(Type), BaseName),
            {ok, State1}  = DbMod:open_db(NextMetric, [{storage_dir, DbDir}]),
            {ok, success} = DbMod:insert_many(State, NewPoints),
            DbMod:delete_many(State, DelPoints),
            aggregate_data(DbDir, DbMod, BaseName, State1)
    end.


merge_points([], _Diff, _MergeFun) ->
    {[], []};
merge_points([{_K,_V}], _Diff, _MergeFun) ->
    {[], []};
merge_points([{K,V} | Rest], Diff, MergeFun) ->
    IntK      = binary_to_integer(K),
    merge_points(Rest, {IntK, Diff}, MergeFun, {[IntK], [V]}, [], [{K,V}], []).


merge_points([], _, _, _, MergeAcc, _DelPoints, DelAcc) ->
    {lists:reverse(MergeAcc), DelAcc};

merge_points([{K,V} | Rest], {_Base, {Type, Interval}}, {Module, Fun},
             {AccK, AccV}, MergeAcc, DelPoints, DelAcc) when Interval =< 1 ->

    NewVal    = apply(Module, Fun, [AccV]),
    AvgKey    = round(graph_utils:mean(AccK)),
    Interval0 = db_utils:get_interval(Type),
    IntK      = binary_to_integer(K),
    Diff      = {Type, Interval0},
    KeyValAcc = {[IntK],[V]},
    MergeAcc0 = [{AvgKey, NewVal} | MergeAcc],

    merge_points(Rest, {IntK, Diff}, {Module, Fun}, KeyValAcc, MergeAcc0, [{K,V}],
                 lists:flatten([DelPoints | DelAcc]));

merge_points([{K,V} | Rest], {Base, {Type, Interval}}, MergeFun, {AccK, AccV},
             MergeAcc, DelPoints, DelAcc) when Interval > 1 ->
    IntK       = binary_to_integer(K),
    Interval0  = IntK - Base,
    Diff       = {Type, Interval - Interval0},
    DelPointsN = [{K,V} | DelPoints],
    KeyValAcc  = {[IntK | AccK], [V | AccV]},
    merge_points(Rest, {IntK, Diff}, MergeFun, KeyValAcc, MergeAcc, DelPointsN,
                 DelAcc).


%% from list of {Key, Val} pairs get the avg different between the consecutive keys
get_granularity(Data) ->
    AvgDiff = db_utils:get_avg_interval(Data),
    if
        AvgDiff < 60     -> {min,  db_utils:get_interval(min)}; % sec;
        AvgDiff < 3600   -> {hour, db_utils:get_interval(hour)}; %min;
        AvgDiff < 86400  -> {day,  db_utils:get_interval(day)};
        true             -> {stop, db_utils:get_interval(day)}
    end.


%%% generate data for testing
generate_data(End, Jump) ->
    Time = db_utils:unix_time(),
    [{integer_to_binary(Time + N), integer_to_binary(crypto:rand_uniform(1000, 9999))} || N <- lists:seq(0, End, Jump)].


