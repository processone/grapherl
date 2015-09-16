-module(db_manager).

-behaviour(gen_server).

%% API functions
-export([start_link/1
        ,get_metric_type/1
        ,get_metric_fd/1
        ,get_metric_fd/2
        ,get_metric_maps/0
        ,get_metric_clients/1
        ,get_all_metric_clients/0
        ,load_metric_map/1
        ,pre_process_metric/1
        ,load_clients/0
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(INDEX, metric_client_index).
-define(SUFFIX, <<"metric">>).
%-record(state, {}).

%%%===================================================================
%%% API functions
%%%===================================================================
get_metric_type(Mn) ->
    [{_, Val}]        = fetch_fd(Mn),
    {metric_type, Mt} = lists:keyfind(metric_type, 1, Val),
    {ok, Mt}.



%%get_metric_fd({Mn, Cn, Mt}) -> {ok, CacheFd, LiveMetricFd}.
get_metric_fd(Mid) ->
    get_metric_fd(Mid, live).



%% metric storing seconds is assumed to be live
get_metric_fd(Mid, sec) ->
    get_metric_fd(Mid, live);

get_metric_fd({Mn, Cn, Mt}, Granularity) ->
    {ok, MapList} = get_metric_maps({Mn, Cn, Mt}, Granularity),
    case graph_utils:get_args(MapList, [cache_fd, {db_fd, Granularity}]) of
        {error_params_missing, _} ->
            gen_server:call(?MODULE, {get_metric_fd, {Mn, Mt}, Granularity});
        {ok, [CacheFd, DbFd]} ->
            {ok, CacheFd, DbFd}
    end.



get_metric_maps({Mn, Cn, Mt}, Granularity) ->
    %case ets:lookup(?MODULE, {Mn, Cn}) of
    % TODO send udpates to graph_db_server reg each incomming cilent
    case fetch_fd(Mn) of
        [] when Granularity =:= live ->
            %io:format("metric don't exits calling gen_server ~p~n", [{Cn, self()}]),
            gen_server:call(?MODULE, {get_metric_fd, {Mn, Mt}, live}),
            update_client_list(Mn, Cn),
            [{_Key, Value}] = fetch_fd(Mn), %ets:lookup(?MODULE, {Mn, Cn}),
            {ok, Value};

        %% if the Granularity req was not for live it means that metric should
        %% have been create by manager earlier but since it is not in its state
        %% so it will throw error
         [] ->
            {false, error};
        [{_Key, Value}] ->
            update_client_list(Mn, Cn),
            {ok, Value}
    end.


%% Cn=false restricts db_worker to add Client name when querying handler
update_client_list(_Mn, false) ->
    ok;
update_client_list(Mn, Cn) ->
    store_clients(Mn, Cn).


load_metric_map({Mn, _Cn}) ->
    case fetch_fd(Mn) of
        [] -> {false, error_no_metric_map};
        [{_Key, Value}] -> {ok, Value}
    end.


%% =========================================================================
%% return all metric maps
%% NOTE: this function shouldn't call db_manger becasue it is being used in
%% termiate() to store state.
load_clients() ->
    {ok, Maps} = db_manager:get_metric_maps(),
    lists:map(
      fun({{MetricName, _}, MetricData}) ->
              {ok, [DbFd, CacheFd]} =
                  graph_utils:get_args(MetricData, [{db_fd, live},
                                                    cache_fd]),
              {ok, Clients0} = db_worker:get_clients(db, DbFd),
              {ok, Clients1} = db_worker:get_clients(cache, CacheFd),
              Clients = lists:usort(lists:flatten([Clients0, Clients1])),
              store_clients(MetricName, Clients)
      end, Maps),
    {ok, success}.



get_metric_maps() ->
    MetricData = lists:filtermap(fun([{K,V}]) -> {true, {K,V}} end, 
                                 ets:match(?MODULE, '$1')),
    {ok, MetricData}.


%% get all client for a given metric
get_metric_clients(Mn) ->
    case ets:match(?INDEX, {{Mn, '$1'}, '_'}) of
        '$end_of_table' ->
            {ok, []};
        List ->
            MCList = lists:flatten(List),
            {ok, MCList}
    end.


%% get clients for all metrics
get_all_metric_clients() ->
    case ets:match(?INDEX, {'$1', '_'}) of
        '$end_of_table' ->
            {ok, []};
        List ->
            MCList = lists:foldl(
                       fun({Mn, Client}, Acc) ->
                               case lists:keyfind(Mn, 1, Acc) of
                                   false -> [{Mn, [Client]} | Acc];
                                   {Mn, Clients} ->
                                       lists:keystore(Mn, 1, Acc,
                                                      {Mn, [Client | Clients]})
                               end
                       end, [], lists:flatten(List)),
            {ok, MCList}
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

    %% spawn an ets table to store metric client list for faster access
    Pid = graph_db_sup:ets_sup_pid(),
    ets:new(?INDEX, [set, public, named_table
                    ,{heir, Pid, none}
                    ,{write_concurrency, false}
                    ,{read_concurrency, true}]),

    {ok, StateSaveTimeout} = application:get_env(graph_db, 
                                                 db_manager_state_dump_timeout),
    timer:send_interval(StateSaveTimeout, {save_state}),
    {ok, #{db          => DbMod
          ,cache       => CacheMod
          ,storage_dir => Dir}}.



%% reload db_manager state from disk
reload_state(DbMod, CacheMod, Dir) ->
    case ets:info(?MODULE) of
        undefined ->
            {ok, success} = load_prev_state(DbMod, CacheMod, Dir);
        _  ->
            {ok, success}
    end.



%% load db_manager ets dump from disk
load_prev_state(DbMod, CacheMod, Dir) ->
    Path = erlang:binary_to_list(<<Dir/binary, "db_manager.dat">>),
    Pid = graph_db_sup:ets_sup_pid(),
    ets:new(?MODULE, [set, public, named_table
                     ,{heir, Pid, none}
                     ,{write_concurrency, false}
                     ,{read_concurrency, true}]),

    case file:open(Path, [read]) of
        {error,enoent} -> {ok, success};
        {ok, Fd} -> init_db_handlers(Fd, {DbMod, CacheMod, Dir})
    end.



%% initalize metric cache from db_manager state
init_db_handlers(Fd, {DbMod, CacheMod, Dir}) ->
    case file:read_line(Fd) of
        {ok, Data} ->
            {ok, [Mn, Mt]} = extract_args(Data),
            Cn             = ?SUFFIX,
            MetricName     = db_utils:to_metric_name({Mn, Cn}),
            bootstrap_metric({Mn, Mt}, MetricName, {DbMod, open_db, live},
                             {CacheMod, open_db}, Dir),

            init_db_handlers(Fd, {DbMod, CacheMod, Dir});
        eof ->
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
handle_call({get_metric_fd, {Mid, Mt}, live}, _From,  State) ->
    #{db          := DbMod
     ,cache       := CacheMod
     ,storage_dir := Dir}  = State,

    MetricName = db_utils:to_metric_name({Mid, ?SUFFIX}),

    %case ets:lookup(?MODULE, {Mid, Cn}) of
    case fetch_fd(Mid) of
        [] ->
            bootstrap_metric({Mid, Mt}, MetricName, {DbMod, init_db, live},
                             {CacheMod, open_db}, Dir),

            [{_Key, Metric}]      = fetch_fd(Mid), %ets:lookup(?MODULE, {Mid,Cn}),
            {ok, [CacheFd, DbFd]} = graph_utils:get_args(Metric, [cache_fd, {db_fd, live}]),
            {reply, {ok, CacheFd, DbFd}, State};

        [{_, MetricData}] ->
            {ok, [CacheFd, DbFd]} = graph_utils:get_args(MetricData,
                                                         [cache_fd, {db_fd, live}]),
            {reply, {ok, CacheFd, DbFd}, State}
    end;

handle_call({get_metric_fd, {Mn, _Mt}, Granularity}, _From,  State) ->
    #{db          := DbMod
     ,storage_dir := Dir}  = State,

    BaseName   = db_utils:to_metric_name({Mn, ?SUFFIX}),
    MetricName = db_utils:get_metric_name(Granularity, BaseName),

    %% we expect that the live db are up and running so we will already have
    %% the {Mid,Cn} entry in the state.
    [{Key, MetricData}] = fetch_fd(Mn), %ets:lookup(?MODULE, {Mn, Cn}),
    {ok, [CacheFd]}     = graph_utils:get_args(MetricData, [cache_fd]),

    case graph_utils:get_args(MetricData, [{db_fd, Granularity}]) of
        {error_params_missing, _Error} ->
            {ok, DbFd}  = create_db(DbMod, init_db, {MetricName, Dir}),
            MetricDataN = lists:keystore({db_fd, Granularity}, 1, MetricData,
                                         {{db_fd,Granularity}, DbFd}),

            store_fd({Key, MetricDataN}),
            %ets:insert(?MODULE, {{Mn, Cn}, MetricDataN}),

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
%% dump metric cache to disk
handle_info({cache_to_disk, MetricId}, State) ->
    %% io:format("~n[+] dump cache to disk state~n", []),
    db_worker:dump_data(MetricId),
    {noreply, State};

handle_info({save_state}, State) ->
    io:format("~n[+] periodically saving db_manager state~n", []),
    dump_state(),
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
            {ok, Metrics} = get_metric_maps(), %ets:match(?MODULE, '$1'),
            CleanMetric   =
                fun({_Key, Value}) ->
                        {ok, [Name
                             ,CacheFd
                             ,DbFd
                             ,Tref]} = graph_utils:get_args(Value, [name
                                                                   ,cache_fd
                                                                   ,{db_fd, live}
                                                                   ,tref]),

                        %% store cache before shutting down
                        timer:cancel(Tref),
                        {ok, Data} = CacheMod:read_all(CacheFd),
                        [DbMod:insert_many(DbFd, Client, Points)  || {Client, Points} <- Data ],
                        CacheMod:close_db(CacheFd),
                        %% if Reason =:= shutdown -> graph_db_sup:ets_sup_stop_child(Name); true -> ok end,
                        graph_db_sup:ets_sup_stop_child(Name),
                        [DbMod:close_db(Fd) || {{db_fd, _Type}, Fd} <- Value]
                        %ets:insert(?MODULE, {Key, {Name, Mtype}})
                end,
            graph_utils:run_threads(8, Metrics, CleanMetric);

        _ ->
            ok
    end,

    %% write to file
    dump_state(),
    if Reason =:= shutdown -> graph_db_sup:stop_ets_sup(); true -> ok end,    
    ok.


dump_state() ->
    %% write to file
    {ok, Dir} = application:get_env(graph_db, storage_dir),
    FilePath  = erlang:binary_to_list(<<Dir/binary, "db_manager.dat">>),
    {ok, FFd} = file:open(FilePath, [write]),
    ets:foldl(
      fun({{Mn, _}, Value}, _Acc) ->

              {ok, [Mt]} = graph_utils:get_args(Value, [metric_type]),
              file:write(FFd, erlang:binary_to_list(
                                <<Mn/binary, ", ", Mt/binary, "\n">>))

      end, 0, ?MODULE),
    file:close(FFd).

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
%% initalize metric ram and db instances
bootstrap_metric({Mid, Mt}, MetricName, {DbMod, DFun, Type}, {CacheMod, CFun}, Dir) ->

    %% create ets table as direct children of graph_db_sup
    {ok, CacheFd} = get_cache_fd(Mid,  {CacheMod, CFun, Dir}),
    {ok, DbFd}    = create_db(DbMod, DFun, {MetricName, Dir}),
    {ok, Timeout} = application:get_env(graph_db, cache_to_disk_timeout),    
    {ok, Tref}    = timer:send_interval(Timeout, {cache_to_disk, {Mid, Mt}}),

    MetricState = {{Mid, fds}, [{{db_fd, Type}, DbFd}
                               ,{name,MetricName}
                               ,{metric_type, Mt}
                               ,{tref, Tref}
                               ,{cache_fd, CacheFd}]},

    ets:insert(?MODULE, MetricState).



get_cache_fd(Mid, {CacheMod, CFun, Dir}) ->
    case ets:match(?MODULE, {{Mid, '_'}, '$1'}, 1) of
        '$end_of_table' ->
            graph_db_sup:init_metric_cache(erlang:binary_to_atom(Mid, utf8),
                                           {CacheMod, CFun, Dir});
        {[[Val]], _} ->
            {cache_fd, CacheFd} = lists:keyfind(cache_fd, 1, Val),
            {ok, CacheFd}
    end.



create_db(Mod, Fun, {MetricName, Dir}) ->
    erlang:apply(Mod, Fun, [ MetricName, [{storage_dir, Dir}] ]).



fetch_fd(Mn) ->
    case ets:match(?MODULE, {{Mn, '$1'}, '$2'}) of
        []            -> [];
        [[Cn, Val]] ->
            [{{Mn, Cn}, Val}]
    end.



store_fd({Key, Val}) ->
    ets:insert(?MODULE, {Key, Val}).




store_clients(Mn, Cn) ->
    ets:insert(?INDEX, {{Mn, Cn}, {}}).


%% pre_process_metric/1 : input is a comma seperated file where each line
%% contains metrics name (eg active_users_online) and the metric type. Eg.
%% === FILE STARTS === 
%% 
%% active_users_online, p
%% cpu_usage, p
%% concurrent_connections, p
%% 
%% === FILE ENDS ===
%%
%% It is advisable to NOT have any space in the metric name.
%% the file name should be absolute path
pre_process_metric(FilePath) ->
    {ok, Fd} = file:open(FilePath, [read]),
    read_file(Fd),
    file:close(Fd).


read_file(Fd) ->
    case file:read_line(Fd) of
        {ok, Data} ->
            %% [RMn, RMt] = string:tokens(string:strip(Data, both, $\n ), ","),
            %% Mn = erlang:list_to_binary(string:strip(RMn)),
            %% Mt = erlang:list_to_binary(string:strip(RMt)),
            {ok, [Mn, Mt]} = extract_args(Data),
            io:format("starting metric ~p ~p~n", [Mn, Mt]),
            db_manager:get_metric_fd({Mn, Mt}),
            read_file(Fd);
        eof ->
            ok
    end.

extract_args(Data) ->
    [RMn, RMt] = string:tokens(string:strip(Data, both, $\n ), ","),
    Mn = erlang:list_to_binary(string:strip(RMn)),
    Mt = erlang:list_to_binary(string:strip(RMt)),
    {ok, [Mn, Mt]}.

%% testing

%% create_metrics(N) ->
%%     lists:map(
%%       fun(I) ->
%%               Cn = list_to_binary("www.server" ++ integer_to_list(I) ++ ".com"),
%%               db_manager:get_metric_fd({<<"cpu_usage">>, Cn, <<"g">>})
%%      end, lists:seq(1, N)).
