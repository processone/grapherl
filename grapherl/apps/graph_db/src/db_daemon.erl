-module(db_daemon).

-behaviour(gen_server).

%% API functions
-export([start_link/1
        ,aggregate_data/4
        ,should_be_purged/2
        ]).



%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("../include/graph_db_records.hrl").

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
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Args], []).


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
init([Args]) ->
    {ok, Timeout} = application:get_env(graph_db, db_daemon_timeout),
    {ok, DbDir}   = application:get_env(graph_db, storage_dir),
    {ok, [DbMod]} = graph_utils:get_args(Args, [db_mod]),

    % TODO timeout 0 -> Timeout
    {ok, #{timeout => Timeout, storage_dir => DbDir, db_mod => DbMod}, 0}.

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
handle_call({interrupt}, _From, State) ->
    {reply, ok, State, 0};

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
%% compress/purge metric data
handle_info(timeout,  State) ->
    #{storage_dir := DbDir, timeout := Timeout, db_mod := DbMod} = State,
    io:format("~n[db_daemon] staring purging~n"),
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

purge_data(DbMod, DbDir, MetricData) ->
    %purge_data(DbMod, DbDir, MetricData, 0),
    PurgeData = fun({Key, Data}) ->
                        {{db_fd, live}, DbFd} = lists:keyfind({db_fd, live}, 1, Data),
                        aggregate_data(DbDir, DbMod, {Key, Data}, {DbFd, live})
                end,
    graph_utils:run_threads(8, MetricData, PurgeData),
    db_utils:gc().


aggregate_data(DbDir, DbMod, {{Mid,Cn}, MetricData}, {DbFd, CurrType}) ->
    %% optimize granularity calculation
    {name, BaseName} = lists:keyfind(name, 1, MetricData),

    {ok, KeyVal}     = DbMod:read_all(DbFd),
    io:format("~n[+] Compressing ~p (length ~p)~n", [BaseName, erlang:length(KeyVal)]),

    %% TODO optimize this
    case should_be_purged(KeyVal, CurrType) of
        {false, stop} ->
            ok;
        {Purge, {Type, Step}} ->
            MergeFun  = {graph_utils, mean},
            NextType  = db_utils:get_next_type(Type),
            DbFdNext  = get_next_db_fd({Mid,Cn}, NextType, MetricData),
            ok        = merge_points(KeyVal, {Type, Step}, #{db_fd      => DbFd
                                                            ,db_fd_next => DbFdNext
                                                            ,db_mod     => DbMod
                                                            ,merge_fun  => MergeFun
                                                            ,purge      => Purge}),
            aggregate_data(DbDir, DbMod, {{Mid,Cn}, MetricData},
                           {DbFdNext, NextType})
    end.


%% get the next db object based on granularity
get_next_db_fd({Mid,Cn}, NextType, MetricData) ->
    case lists:keyfind({db_fd, NextType}, 1, MetricData) of
        {{db_fd, NextType}, NextFd} ->
            NextFd;
        _ ->
            {ok, _, NextFd}  = db_manager:get_metric_fd(Mid, Cn, NextType),
            NextFd
    end.


%% compression and purging routine
merge_points([], _Diff, _Args) ->
    ok;
merge_points([{_K,_V}], _Diff, _Args) ->
    ok;
merge_points([{K,V} | Rest], Diff, Args) ->
    IntK      = erlang:binary_to_integer(K),
    merge_points(Rest, {IntK, Diff}, Args, {[IntK], [V]}, [{K,V}]).


merge_points([], _, _Args, _KVAcc, _DelPoints) ->
    ok;
merge_points([{K,V} | Rest], {_Base, {Type, Interval}}, Args,
             {AccK, AccV}, DelPoints) when Interval =< 0 ->

     #{db_fd_next := DbFdNext
      ,db_fd      := DbFd
      ,db_mod     := DbMod
      ,merge_fun  := {Module, Fun}
      ,purge      := Purge} = Args,

    NewVal    = erlang:apply(Module, Fun, [AccV]),
    AvgKey    = erlang:integer_to_binary(
                  erlang:round(
                    graph_utils:binary_to_realNumber(
                      graph_utils:mean(AccK)))),

    {ok, success} = DbMod:insert(DbFdNext, {AvgKey, NewVal}),
    if Purge =:= true -> {ok, success} = DbMod:delete_many(DbFd, DelPoints); true -> ok end,

    Interval0 = db_utils:get_interval(Type),
    Diff      = {Type, Interval0},
    IntK      = erlang:binary_to_integer(K),
    KVAcc     = {[IntK],[V]},

    merge_points(Rest, {IntK, Diff}, Args, KVAcc, [{K,V}]);

merge_points([{K,V} | Rest], {Base, {Type, Interval}}, Args,
             {AccK, AccV}, DelPoints) when Interval > 0 ->
    IntK       = erlang:binary_to_integer(K),
    Interval0  = IntK - Base,
    Diff       = {Type, Interval - Interval0},
    KVAcc      = {[IntK | AccK], [V | AccV]},

    merge_points(Rest, {IntK, Diff}, Args, KVAcc, [{K,V} | DelPoints]).


%% check if there are enough data points to be purged
should_be_purged(KeyVal, Type) ->
    case get_granularity(KeyVal, Type) of
        {next, stop} -> {false, stop};
        {Granularity, Step} ->
            Size    = db_utils:get_aggregation_size(Granularity),
            io:format("[+] compressing ~p to ~p~n", [Granularity, db_utils:get_next_type(Granularity)]),
            {Hd, _} = erlang:hd(KeyVal),
            {Tl, _} = erlang:hd(lists:reverse(KeyVal)),
            Diff    = erlang:abs(graph_utils:binary_to_realNumber(Hd) -
                                     graph_utils:binary_to_realNumber(Tl)),

            %io:format("[+] Hd : ~p, Tl: ~p~n", [Hd, Tl]),
            io:format("[+] Required time interval: ~p~n[+] Available time interval : ~p~n", [Size, Diff]),
            Ret = if Diff >= Size -> true; true -> false end,
            {Ret, {Granularity, Step}}
    end.


%% from list of {Key, Val} pairs get the avg different between the consecutive keys
get_granularity(Data, live) ->
    case db_utils:get_avg_interval(Data) of
        error   -> {next, stop};
        AvgDiff ->
            %io:format("[+] Calculating granularity: ~p sec~n", [AvgDiff]),
            if
                AvgDiff < 60     -> {?SEC, db_utils:get_interval(?SEC)}; % sec;
                AvgDiff < 3600   -> {?MIN, db_utils:get_interval(?MIN)}; %min;
                AvgDiff < 86400  -> {?HOUR,  db_utils:get_interval(?HOUR)};
                true             -> {next, stop}
            end
    end;
get_granularity(_Data, ?DAY) ->
    {next, stop};
get_granularity(Data, Type) when Data =/= []->
    {Type, db_utils:get_interval(Type)};
get_granularity(_, _) ->
    {next, stop}.

%%% generate data for testing
%% generate_data(End, Jump) ->
%%     Time = db_utils:unix_time(),
%%     [{integer_to_binary(Time + N), integer_to_binary(crypto:rand_uniform(1000, 9999))} || N <- lists:seq(0, End, Jump)].


