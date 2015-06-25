-module(db_worker).


-behaviour(gen_server).

%% Api functions
-export([start_link/1,
         store/1,
         retrive/1,
         retrive/2,
         dump_data/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("apps/grapherl/include/grapherl.hrl").

%-record(state, {db_mod, cache_mod}).

%%%===================================================================
%%% API functions
%%%===================================================================

%% store data point in cache db
store(Data) ->
    poolboy:transaction(?DB_POOL,
                        fun(Worker) ->
                                gen_server:cast(Worker, {store, Data})
                        end).

%% get data points for Metric from cache and databse.
retrive(MetricName) ->
    poolboy:transaction(?DB_POOL,
                        fun(Worker) ->
                                gen_server:call(Worker, {read_metric, MetricName})
                        end).

retrive(MetricName, Granularity) ->
    poolboy:transaction(?DB_POOL,
                        fun(Worker) ->
                                gen_server:call(Worker, {read_metric, MetricName, Granularity})
                        end).    

%% dump cache to databse after TIMEOUT
dump_data(MetricId) ->
    poolboy:transaction(?DB_POOL,
                      fun(Worker) ->
                              gen_server:cast(Worker, {dump_to_disk, MetricId})
                      end).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link(?MODULE, [Args], []).

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
    ?INFO("~p starting with args: ~p ~n", [?MODULE, Args]),
    case graph_utils:get_args(Args, [db_mod, cache_mod]) of
        {ok, [DbMod, CacheMod]} ->
            {ok, #{db_mod => DbMod, cache_mod => CacheMod, id => self()}};
        _ ->
            ?ERROR("ERROR(~p): no UDP socket found.~n", [?MODULE]),
            {stop, no_socket}
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
handle_call({read_metric, {Mn, Cn}}, _From, #{db_mod := Db, cache_mod := Cache} = State) ->
    %% TODO read in chunks and keep sending the data.
    {ok, CacheFd, DbFd}  = db_manager:get_metric_fd(Mn, Cn),
    {ok, CacheData} = Cache:read_all(CacheFd),
    {ok, DbData}    = Db:read_all(DbFd),
    {reply, lists:flatten([CacheData | DbData]), State};

handle_call({read_metric, {Mn, Cn}, Granularity}, _From, #{db_mod := Db, cache_mod := Cache} = State) ->
    %% TODO read in chunks and keep sending the data.
    {ok, CacheFd, DbFd}  = db_manager:get_metric_fd(Mn, Cn, Granularity),
    {ok, CacheData}      = Cache:read_all(CacheFd),
    {ok, DbData}         = Db:read_all(DbFd),
    {reply, lists:flatten([CacheData | DbData]), State};

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
handle_cast({store, #packet{mn=Mn, cn=Cn, mp={Key,Val}}}, #{cache_mod := Cache} = State) ->
    {ok, CacheFd, _} = db_manager:get_metric_fd(Mn, Cn),
    {ok, _}          = Cache:insert(CacheFd, {Key, Val}),
    {noreply, State};

handle_cast({dump_to_disk, {Mn, Cn}}, #{db_mod := Db, cache_mod := Cache} = State) ->
    {ok, CacheFd, DbFd}  = db_manager:get_metric_fd(Mn, Cn),
    {ok, Data}           = Cache:read_all(CacheFd),
    io:format("~n[+] Writing cache to disk. (Size ~p) ~n", [erlang:length(Data)]),
    {ok, success}        = Db:insert_many(DbFd, Data),
    {ok, success}        = Cache:clear_db(CacheFd),
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

%%%===================================================================
%%% Internal functions
%%%===================================================================
