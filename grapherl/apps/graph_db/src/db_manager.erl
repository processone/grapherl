-module(db_manager).

-behaviour(gen_server).

%% API functions
-export([start_link/1
        ,init_cache/2
        ,init_db/2
        ,get_cache_metric/2
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

get_cache_metric(Mid, Cn) ->
    gen_server:call(?MODULE, {get_cache_metric, Mid, Cn}).    


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
    
    {ok, MapList} = load_prev_state(),
    {ok, [DbMod, CacheMod]} = graph_utils:get_args(Args, [db_mod, cache_mod]),
    {ok, #{db => DbMod, cache => CacheMod, metric_maps => MapList}}.


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
handle_call({get_cache_metric, Mid, Cn}, _From,  State) ->
    #{metric_maps := List} = State,
    #{db := DbMod}         = State,
    #{cache := CacheMod}   = State,

    MetricName = <<Mid/binary, "-", Cn/binary>>,

    %% CAUTION when db_manager fails it will have no state of which db had been
    %% created so we need to figure out how to bring db_manager up to the mark
    case lists:keyfind({Mid, Cn}, 1, List) of
        false ->
            {ok, _} = CacheMod:init_db(MetricName, []),
            {ok, _} = DbMod:init_db(MetricName, []),
            ListNew = lists:keystore({Mid, Cn}, 1, List, {{Mid, Cn}, MetricName}),
            {reply, {ok, MetricName, State}, State#{metric_maps => ListNew}};

        {_, MetricName} ->
            {reply, {ok, MetricName}, State}
    end;

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
    ets:insert(db_manager, {metric_maps, MapList}),
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
