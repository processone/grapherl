-module(db_worker).
-behaviour(gen_server).

%% Api functions
-export([start_link/1
        %,store/2
        ,direct_store/1
        ,read_cache/1
        ,read_db/1
        ,dump_data/1
        ,store_batch/3
        ,prepare_batch/2
        ,get_clients/2
        ]).

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
%% store data point in metric cache
direct_store(Data) ->
    poolboy:transaction(?DB_POOL,
                        fun(Worker) ->
                                gen_server:cast(Worker, {direct_store, Data})
                        end).

read_cache({Cn, Fd, Range}) ->
    poolboy:transaction(?DB_POOL,
                        fun(Worker) ->
                                gen_server:call(Worker, {read_cache, Cn, Fd, Range})
                        end).    


read_db({Cn, Fd, Range}) ->
    poolboy:transaction(?DB_POOL,
                        fun(Worker) ->
                                gen_server:call(Worker, {read_db, Cn, Fd, Range})
                        end). 

%% dump cache to databse after TIMEOUT
dump_data(MetricId) ->
    poolboy:transaction(?DB_POOL,
                      fun(Worker) ->
                              gen_server:cast(Worker, {dump_to_disk, MetricId})
                      end).

get_clients(Type, DbFd) ->
    poolboy:transaction(?DB_POOL,
                        fun(Worker) ->
                                gen_server:call(Worker,
                                                {get_metric_clients, Type, 
                                                 DbFd})
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

    case graph_utils:get_args(Args, [db_mod, cache_mod, ports]) of
        {ok, [DbMod, CacheMod, Ports]} ->
            %% garbage collect after 100 sec
            %%{ok, _Tref2} = timer:send_interval(1000, {process_queue}),
            {ok, Block} = application:get_env(graph_db, msg_queue_block),
            {ok, #{db_mod      => DbMod
                  ,cache_mod   => CacheMod
                  ,id          => self()
                  ,block_size  => Block
                  ,ports       => Ports
                  ,cache       => []
                  ,batch       => []
                  }, 0};

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
handle_call({read_cache, Cn, CacheFd, {Start, End}}, _From,
            #{cache_mod := Cache} = State) ->
    {ok, Data} = Cache:get_range(CacheFd, {Cn, Start, End}),
    {reply, {ok, Data}, State};

handle_call({read_db, Cn, DbFd, {Start, End}}, _From,
            #{db_mod := Db} = State) ->
    {ok, Data} = Db:get_range(DbFd, {Cn, Start, End}),
    {reply, {ok, Data}, State};

handle_call({get_metric_clients, Type, Fd}, _From,
            #{db_mod := Db, cache_mod := Cache} = State) ->
    
    {ok, Clients} = case Type of
                        db -> Db:get_clients(Fd);
                        cache -> Cache:get_clients(Fd)
                    end,
    {reply, {ok, Clients}, State};

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
%% store data from message queue in the respective metric cache tables
handle_cast({direct_store, Packet}, #{cache_mod := CacheMod, cache := Cache} = State) ->
    case binary:split(Packet, [<<"/">>, <<":">>], [global]) of
        [Cn, Mn, Mt, Key, Val] ->
            case db_manager:get_metric_fd({Mn, Cn, Mt}) of
                {ok, CacheFd, _} ->
                    {ok, _} = CacheMod:insert(CacheFd, Cn, {Key, Val}),
                    {noreply, State};
                _ ->
                    {noreply, State#{cache => [ [{Mn,Cn,Mt}, {Key,Val}]  | Cache] }, 500}
            end;

        _ ->
            false
    end;

%% dump metric cache to disk
handle_cast({dump_to_disk, {Mn, Cn, Mt}}, #{db_mod := Db, cache_mod := Cache} = State) ->
    {ok, CacheFd, DbFd}  = db_manager:get_metric_fd({Mn, Cn, Mt}),
    {ok, Data}           = Cache:read(CacheFd, Cn),
    io:format("~n[+] Writing cache to disk. (Size ~p) ~n", [erlang:length(Data)]),
    {ok, success}        = Db:insert_many(DbFd, Cn, Data),
    {ok, success}        = Cache:clear_client(CacheFd, Cn),
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
handle_info(timeout, #{cache_mod := CacheMod, cache := Data,
                       batch := PrevBatch} = State) ->
    {ok, Batch}    = prepare_batch(Data, []),
    {ok, BatchNew} = store_batch(none, lists:flatten([Batch, PrevBatch]), CacheMod),
    db_utils:gc(),
    {noreply, State#{cache => [], batch => BatchNew}, 5000};

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
store_batch(_Tab, [], _) ->
    {ok, []};
store_batch(Tab, [{{Mn, Cn, Mt}, Data} | Rest], Cache) ->
    try
        {ok, CacheFd, _} = db_manager:get_metric_fd({Mn, Cn, Mt}),
        {ok, _}          = Cache:insert_many(CacheFd, Cn, Data),
        store_batch(Tab, Rest, Cache)
    catch
        _:_ ->
            io:format("[-] error occured in db_worker:store_batch ~p~n", [erlang:get_stacktrace()]),
            {ok, Rest}
    end.


prepare_batch([], Acc) ->
    {ok, lists:reverse(Acc)};
prepare_batch([[{Mn, Cn, Mt}, {Key, Val}] | Rest], Acc) ->
    case lists:keyfind({Mn, Cn, Mt}, 1, Acc) of
        false ->
            prepare_batch(Rest, [{{Mn, Cn, Mt}, [{Key, Val}]} | Acc]);
        {{Mn, Cn, Mt}, Data} ->
            prepare_batch(Rest, lists:keystore({Mn, Cn, Mt}, 1, Acc,
                                               {{Mn, Cn, Mt}, [{Key, Val} | Data]}))
    end.
