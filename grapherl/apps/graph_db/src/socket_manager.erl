-module(socket_manager).
-author('kansi13@gmail.com').

-behaviour(gen_server).

%% API functions
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("apps/grapherl/include/grapherl.hrl").

%% will store a list of sockets that are being using to receive metric from
%% clients
-record(state, {sockets, type}).

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
start_link({type, Type}) ->
    proc_lib:start_link(?MODULE, init, [{type, Type}]).
    %gen_server:start_link({local, ?MODULE}, ?MODULE, [Args], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server. Create UDP socket for receiving metric and TCP
%% socket for handling metric requests.
%% Type : r (router i.e receive metric and router to gen_db) |
%%        s (TCP server serving metric data)
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([{type, Type}]) ->
    process_flag(trap_exit, true),
    ok = proc_lib:init_ack({ok, self()}),
    ?INFO("~p(~p): Starting~n", [?MODULE, "default"]),

    %% start default TCP/UDP ports for handling requests and receiving data.
    {ok, State} = init_default(Type, #state{}),
    gen_server:enter_loop(?MODULE, [], State#state{type=Type}, 0).

init_default(?ROUTER, State) ->
    {ok, State} = init_socket(udp, ?R_PORT, ?ROUTER, State),
    {ok, State};
init_default(?DSRV, State) ->
    {ok, State}  = init_socket(tcp, ?S_PORT, ?DSRV, State),
    {ok, State}.

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
handle_info(timeout, State) ->
    %% even though graph_socket_manager is started after ROUTER_SUP and
    %% REQ_HANDLER_SUP servers we use precaution to let the supervisors start
    %% before calling them
    timer:sleep(1000),

    %% start routers and server for receiving data and handling requests.
    init_workers(State#state.type, State#state.sockets),

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
terminate(_Reason, _State) ->
    %% TODO Close all sockets.
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

%% SockHandlerType: r (router) | s (server)
init_socket(PortType, Port, SockHandlerType, State) ->
    {ok, SocketR} = graph_utils:open_port(PortType, Port),
    R_Socket      = [{node, node()}, {socket, SocketR}, {port, Port},
                     {type, SockHandlerType}],
    NewSockList   = [R_Socket | State#state.sockets],
    {ok, State#state{sockets=NewSockList}}.

init_workers(Type, SocketList) when Type =:= ?ROUTER ->
    {ok, RouterNum} = application:get_env(num_routers),
    init_workers0(?ROUTER_SUP, Type, SocketList, RouterNum);

init_workers(Type, SocketList) when Type =:= ?DSRV ->
    {ok, ServerNum} = application:get_env(num_servers),
    init_workers0(?REQ_HANDLER_SUP, Type, SocketList, ServerNum).

init_workers0(SERVER, Type, SocketList, Count) ->
    {ok, Socket} = graph_utils:get_socket(Type, SocketList),
    [ supervisor:start_child(SERVER, [{socket, Socket}, {type, Type}])
      || _N <- lists:seq(1,Count)].

