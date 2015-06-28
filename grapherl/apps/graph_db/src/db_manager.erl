-module(db_manager).

-behaviour(gen_server).

%% API functions
-export([start_link/1
        ,get_metric_fd/2
        ,get_metric_fd/3
        ,get_metric_maps/0
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%-record(state, {}).

%%%===================================================================
%%% API functions
%%%===================================================================
get_metric_fd(Mid, Cn) ->
    get_metric_fd(Mid, Cn, live).


%% metric storing seconds is assumed to be live
get_metric_fd(Mid, Cn, sec) ->
    get_metric_fd(Mid, Cn, live);

get_metric_fd(Mid, Cn, Granularity) ->
    {ok, MapList} = get_metric_maps({Mid, Cn}, Granularity),
    case graph_utils:get_args(MapList, [cache_fd, {db_fd, Granularity}]) of
        {error_params_missing, _} ->
            %% io:format("~n metric don't exits calling gen_server ~p ~p~n", [Granularity, MapList]),
            gen_server:call(?MODULE, {get_metric_fd, Mid, Cn, Granularity});
        {ok, [CacheFd, DbFd]} ->
            {ok, CacheFd, DbFd}
    end.


%% return all metric maps
get_metric_maps() ->
    MetricData = lists:map(fun([{K,V}]) -> {K,V} end, ets:match(?MODULE, '$1')),
    {ok, MetricData}.


get_metric_maps({Mid, Cn}, Granularity) ->
    case ets:lookup(?MODULE, {Mid,Cn}) of
        [] when Granularity =:= live ->
            %% io:format("~n from here metric don't exits calling gen_server ~p~n", [Granularity]),
            gen_server:call(?MODULE, {get_metric_fd, Mid, Cn, live}),
            [{_Key, Value}] = ets:lookup(?MODULE, {Mid,Cn}),
            {ok, Value};

        %% if the Granularity req was not for live it means that metric should
        %% have been create by manager earlier but since it is not in its state
        %% so it will throw error
        [] ->
            {false, error};
        [{_Key, Value}] ->
            {ok, Value}
    end.



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
    process_flag(trap_exit, true),
    
    {ok, Dir} = application:get_env(graph_db, storage_dir),
    {ok, [DbMod, CacheMod]} = graph_utils:get_args(Args, [db_mod, cache_mod]),
    
    %% db_manager stores metric meta data in ets table and saves it to disk
    %% for persistance and to handle crash.
    reload_state(DbMod, CacheMod, Dir),

    {ok, #{db          => DbMod
          ,cache       => CacheMod
          ,storage_dir => Dir}}.

reload_state(DbMod, CacheMod, Dir) ->
    case ets:info(?MODULE) of
        undefined ->
            {ok, success} = load_prev_state(),
            init_db_handlers(DbMod, CacheMod, Dir);
        _  ->
            {ok, success}
    end.

init_db_handlers(DbMod, CacheMod, Dir) ->
    ets:foldl(
      fun({Key, MetricName}, Acc) ->
              bootstrap_metric(Key, MetricName, {DbMod, open_db, live},
                               {CacheMod, open_db}, Dir),
              Acc
      end, 0, ?MODULE).

load_prev_state() ->
    case ets:file2tab("db_manager.dat") of 
        {error,{read_error,{file_error, _, enoent}}} ->
            Pid = graph_db_sup:ets_sup_pid(),
            ets:new(?MODULE, [set, public, named_table,
                              {heir, Pid, none},
                              {write_concurrency, false},
                              {read_concurrency, true}]),
            {ok, success};

        {ok, _Tab} ->
            {ok, success}
    end.


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

%% metric maps is of the format: [{{Mid, Cn},  MetricName}, ...]
%% creates a cache db if it doesn't exist already else return the state of the
%% cache metric.
handle_call({get_metric_fd, Mid, Cn, live}, _From,  State) ->
    #{db          := DbMod
     ,cache       := CacheMod
     ,storage_dir := Dir}  = State,

    MetricName = db_utils:to_metric_name({Mid, Cn}),

    case ets:lookup(?MODULE, {Mid, Cn}) of
        [] ->
            bootstrap_metric({Mid, Cn}, MetricName, {DbMod, init_db, live},
                                       {CacheMod, init_db}, Dir),

            [{_Key, Metric}]      = ets:lookup(?MODULE, {Mid,Cn}),
            {ok, [CacheFd, DbFd]} = graph_utils:get_args(Metric, [cache_fd, {db_fd, live}]),
            {reply, {ok, CacheFd, DbFd}, State};

        [{_, MetricData}] ->
            {ok, [CacheFd, DbFd]} = graph_utils:get_args(MetricData, [cache_fd, {db_fd, live}]),
            {reply, {ok, CacheFd, DbFd}, State}
    end;

handle_call({get_metric_fd, Mid, Cn, Granularity}, _From,  State) ->
    #{db          := DbMod
     ,storage_dir := Dir}  = State,

    BaseName   = db_utils:to_metric_name({Mid, Cn}),
    MetricName = db_utils:get_metric_name(Granularity, BaseName),

    %% we expect that the live db are up and running so we will already have
    %% the {Mid,Cn} entry in the state.
    [{_, MetricData}]  = ets:lookup(?MODULE, {Mid, Cn}),
    {ok, [CacheFd]}    = graph_utils:get_args(MetricData, [cache_fd]),

    case graph_utils:get_args(MetricData, [{db_fd, Granularity}]) of
        {error_params_missing, _Error} ->
            {ok, DbFd}  = create_db(DbMod, init_db, {MetricName, Dir}),
            MetricDataN = lists:keystore({db_fd, Granularity}, 1, MetricData,
                                         {{db_fd,Granularity}, DbFd}),
            case is_binary(MetricDataN) of
                true -> erlang:error("~n~n~n~n~n~n~n~[+] duak just happen~n~n~n~n~n~n~n~n~n");
                _ -> ok
            end,

            ets:insert(?MODULE, {{Mid,Cn}, MetricDataN}),
            {reply, {ok, CacheFd, DbFd}, State};

        {ok, [DbFd]} ->
            {reply, {ok, CacheFd, DbFd}, State}
    end;

%% handle_call({get_metric_maps}, _From, State) ->
%%     MapList = ets:match(?MODULE, '$1'),
%%     {reply, {ok, MapList}, State};

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
handle_info({cache_to_disk, MetricId}, State) ->
    db_worker:dump_data(MetricId),
    {noreply, State};

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
terminate(Reason, #{db := DbMod, cache := CacheMod}) ->
    %% write state to disk while crashing or terminating
    io:format("~n[+] Saving db_manager state ... reason ~p~n", [Reason]),
    case Reason of
        shutdown ->
            ets:foldl(
              fun({Key, Value}, Acc) ->
                      {ok, [Name
                           ,CacheFd
                           ,DbFd
                           ,Tref]} = graph_utils:get_args(Value, [name
                                                                 ,cache_fd
                                                                 ,{db_fd, live}
                                                                 ,tref]),
                      
                      %% store cache before crashing or terminating
                      {ok, Data}    = CacheMod:read_all(CacheFd),
                      {ok, success} = DbMod:insert_many(DbFd, Data),
                      CacheMod:close_db(CacheFd),
                      if Reason =:= shutdown -> graph_db_sup:ets_sup_stop_child(Name); true -> ok end,

                      timer:cancel(Tref),
                      [DbMod:close_db(Fd) || {{db_fd, _Type}, Fd} <- Value],
                      ets:insert(?MODULE, {Key, Name}),
                      Acc

              end, 0, ?MODULE);
        _ ->
            ok
    end,
    ets:tab2file(?MODULE, "db_manager.dat"),
    if Reason =:= shutdown -> graph_db_sup:stop_ets_sup(); true -> ok end,    
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

bootstrap_metric(Key, MetricName, {DbMod, DFun, Type}, {CacheMod, CFun}, Dir) ->
    %% {ok, CacheFd} = create_db(CacheMod, CFun, {MetricName, Dir}),
    %% create ets table as direct children of graph_db_sup
    {ok, CacheFd} = graph_db_sup:init_metric_cache(erlang:binary_to_atom(MetricName,utf8),
                                                   {CacheMod, CFun, Dir}),

    {ok, DbFd}    = create_db(DbMod, DFun, {MetricName, Dir}),
    {ok, Timeout} = application:get_env(graph_db, cache_to_disk_timeout),    
    {ok, Tref}    = timer:send_interval(Timeout, {cache_to_disk, Key}),

    MetricState = {Key, [{{db_fd, Type}, DbFd},
                         {name,MetricName},
                         {tref, Tref},
                         {cache_fd, CacheFd}]},

    ets:insert(?MODULE, MetricState).


create_db(Mod, Fun, {MetricName, Dir}) ->
    erlang:apply(Mod, Fun, [ MetricName, [{storage_dir, Dir}] ]).
