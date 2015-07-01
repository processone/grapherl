-module(msg_queue_processor).

-behaviour(gen_server).

%% API functions
-export([start_link/1
        ,flush_queue/0
        ]).

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
flush_queue() ->
    gen_server:cast(?MODULE, {flush_queue}).

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
    {ok, [Ports]} = graph_utils:get_args(Args, [ports]),
    {ok, Block}   = application:get_env(graph_db, msg_queue_block),
    MsgQs = lists:map(
             fun(Port) ->
                     MsgQ = db_utils:get_msg_queue_name(Port),
                     ets:new(MsgQ, [set, public, named_table, duplicate_bag,
                                    {write_concurrency, true},
                                    {read_concurrency, true}]),
                     MsgQ
             end, Ports),
    {ok, #{block_size => Block, msg_queue => MsgQs}, 0}.

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
handle_cast({flush_queue}, #{msg_queue := List} = State) ->
    lists:map(
     fun(Tab) ->
             ets:delete_all_objects(Tab)
     end, List),
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

%% fetch chunks from the message queue and send then to db worker for
%% processing
handle_info(timeout, #{block_size := BlockSize, msg_queue := List} = State) ->
    lists:map(
      fun(Tab) ->
              case ets:match(Tab, '$1', BlockSize) of
                  '$end_of_table'  ->
                      ok;

                  {DataNested, _}  ->
                      Data = lists:flatten(DataNested),
                      db_worker:store(Data),
                      lists:map(fun(Obj) -> ets:delete_object(Tab, Obj) end, Data)
              end
      end, List),
    db_utils:gc(),
    {noreply, State, 200};

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
