-module(db_manager).

-behaviour(gen_server).

%% API functions
-export([start_link/1
        ,init_cache/2
        ,init_db/2
        ,get_metric_fd/2
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
init_cache(MetricName, Args) ->
    gen_server:call(?MODULE, {init_cache, MetricName, Args}).

init_db(MetricName, Args) ->
    gen_server:call(?MODULE, {init_db, MetricName, Args}).

get_metric_fd(Mid, Cn) ->
    gen_server:call(?MODULE, {get_metric_fd, Mid, Cn}).    

get_metric_maps() ->
    gen_server:call(?MODULE, {get_metric_maps}).


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

    {ok, MetricMaps}   = load_prev_state(),
    {ok, MapList}      = init_db_handlers(MetricMaps, DbMod, CacheMod, Dir, []),
    {ok, #{db          => DbMod
          ,cache       => CacheMod
          ,metric_maps => MapList
          ,storage_dir => Dir}}.

init_db_handlers([], _, _, _, Acc) ->
    {ok, Acc};
init_db_handlers([{Key, MetricName} | Rest], DbMod, CacheMod, Dir, Acc) ->
    AccNew = bootstrap_metric(Key, MetricName, {DbMod, open_db},
                              {CacheMod, init_db}, Dir, Acc),
    init_db_handlers(Rest, DbMod, CacheMod, Dir, AccNew).


load_prev_state() ->
    case ets:file2tab("db_manager.dat") of 
        {error,{read_error,{file_error, _, enoent}}} ->
            ets:new(db_manager, [set, public, named_table,
                                 {write_concurrency, true},
                                 {read_concurrency, false}]),
            {ok, []};
        
        {ok, _Tab} ->
            [{metric_maps, Value}] = ets:lookup(db_manager, metric_maps),
            {ok, Value}
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
handle_call({init_db, MetricName, Args}, _From, #{db := DbMod} = State) ->
    Reply = DbMod:init_db(MetricName, Args),
    {reply, Reply, State};

handle_call({init_cache, MetricName, Args}, _From, #{cache := CacheMod} = State) ->
    Reply = CacheMod:init_db(MetricName, Args),
    {reply, Reply, State};

%% metric maps is of the format: [{{Mid, Cn},  MetricName}, ...]
%% creates a cache db if it doesn't exist already else return the state of the
%% cache metric.
handle_call({get_metric_fd, Mid, Cn}, _From,  State) ->
    #{metric_maps := List
     ,db          := DbMod
     ,cache       := CacheMod
     ,storage_dir := Dir}  = State,

    MetricName = <<Mid/binary, "-", Cn/binary>>,

    %% CAUTION when db_manager fails it will have no state of which db had been
    %% created so we need to figure out how to bring db_manager up to the mark
    case lists:keyfind({Mid, Cn}, 1, List) of
        false ->
            ListNew = bootstrap_metric({Mid, Cn}, MetricName, {DbMod, init_db},
                                       {CacheMod, init_db}, Dir, List),
            {ok, [Metric]}        = graph_utils:get_args(ListNew, [{Mid, Cn}]),
            {ok, [CacheFd, DbFd]} = graph_utils:get_args(Metric, [cache_fd, db_fd]),
            {reply, {ok, CacheFd, DbFd}, State#{metric_maps => ListNew}};

        {_, MetricData} ->
            {ok, [CacheFd, DbFd]} = graph_utils:get_args(MetricData, [cache_fd, db_fd]),
            {reply, {ok, CacheFd, DbFd}, State}
    end;

handle_call({get_metric_maps}, _From, #{metric_maps := MapList} = State) ->
    {reply, {ok, MapList}, State};

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
terminate(_Reason, #{metric_maps := MapList}) ->
    %% write state to disk while crashing
    %% TODO close also db fd's
    MetricMaps = lists:map(
                   fun({Key, Value}) ->
                           {ok, [Name]} = graph_utils:get_args(Value, [name]),
                           {Key, Name}
                   end, MapList),

    ets:insert(db_manager, {metric_maps, MetricMaps}),
    ets:tab2file(db_manager, "db_manager.dat"),
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

bootstrap_metric(Key, MetricName, {DbMod, DFun}, {CacheMod, CFun}, Dir, List) ->
    {ok, CacheFd} = erlang:apply(CacheMod, CFun, [ MetricName, [{storage_dir, Dir}] ]),
    {ok, DbFd}    = erlang:apply(DbMod, DFun, [ MetricName, [{storage_dir, Dir}] ]),
    {ok, Timeout} = application:get_env(graph_db, cache_to_disk_timeout),    
    {ok, Tref}    = timer:send_interval(Timeout, {cache_to_disk, Key}),

    lists:keystore(Key, 1, List, {Key, [{name,MetricName},
                                        {db_fd, DbFd},
                                        {tref, Tref},
                                        {cache_fd, CacheFd}]}).

