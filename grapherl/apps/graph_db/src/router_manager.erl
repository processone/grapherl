-module(router_manager).
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

-define(MAX_ATTEMPS, 5).

%% will store a list of sockets that are being using to receive metric from
%% clients
-record(state, {sockets=[], type, attempts, counter = 0}).

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
init([Args]) ->
    {ok, [PortList]} = graph_utils:get_args(Args, [ports]),

    ?INFO("~p(~p): Starting with args ~p~n", [?MODULE, self(), PortList]),
    %% start default UDP ports for handling requests and receiving data.
    {ok, State} = init_router_socket(PortList, #state{}),
    {ok, State, 0}.



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
%% initalize router_worker processes to handle metric data points
handle_info(timeout, State) ->
    try init_workers(State#state.sockets) of
        _ ->
            {noreply, State}
    catch
        _:Error ->
            io:format("[-] Error occuered while starting router_workers: ~p~n~p~n", [Error, erlang:get_stacktrace()]),
            if 
                State#state.attempts > ?MAX_ATTEMPS ->
                    {stop, max_attemps_exceeded, State};
                true ->
                    {noreply, State, 1000}
            end
    end;
handle_info(Info, State) ->
    io:format("[+] router_manager received unkown message: ~p~n", [Info]),
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
terminate(_Reason, State) ->
    Sockets = State#state.sockets,
    ?INFO("~p stopping ~p ~n", [?MODULE, Sockets]),
    lists:map(fun(SocketData) ->
                      {socket, Socket} = lists:keyfind(socket, 1, SocketData),
                      procket:close(Socket)
                      %gen_udp:close(Socket)
              end, Sockets),
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
%% initalize sockets
init_router_socket([], State) ->
    {ok, State};
init_router_socket([Port | Rest], State) ->
    %{ok, SocketR} = graph_utils:open_port(udp, Port),
    ?INFO("Opening socket with procket : ~p ~n", [Port]),
    {ok, SocketR} = procket:open(Port, [{protocol, udp}, {type, dgram}, {family, inet}]),
    R_Socket      = [{node, node()}, {socket, SocketR}, {port, Port},
                     {type, ?ROUTER}],
    NewSockList   = [R_Socket | State#state.sockets],
    init_router_socket(Rest, State#state{sockets=NewSockList}).


%% initalize router_worker processes for each open port
init_workers([]) ->
    ok;
init_workers([SocketData | Rest]) ->
    {ok, RouterNum}  = application:get_env(num_routers),
    {socket, Socket} = lists:keyfind(socket, 1, SocketData),
    {port, Port}     = lists:keyfind(port, 1, SocketData),

    lists:map(
      fun(_N) ->
              MsgQ = db_utils:get_msg_queue_name(Port),
              supervisor:start_child(?ROUTER_WORKER_SUP, [[{socket, Socket},
                                                           {msg_queue, MsgQ}]])
      end, lists:seq(1,RouterNum)),

    init_workers(Rest).

