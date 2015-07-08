%%%-------------------------------------------------------------------
%%% @author Mickaël Rémond <mremond@process-one.net>
%%% @copyright (C) 2015, ProcessOne
%%% @doc
%%%  add to ejabberd.yml modules section:
%%% modules:
%%% ...
%%%   mod_metrics_test: {}
%%% @end
%%% Created :  1 Jul 2015 by Mickaël Rémond <mremond@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_metrics_test).

-behaviour(gen_mod).
-behaviour(gen_server).

%% API
-export([start_link/2, start/2, stop/1]).

%% Hooks
-export([user_send_packet/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("logger.hrl").

-define(PROCNAME, ?MODULE).
-define(INTERVAL, 5000).
-record(state, {host = <<"">> :: binary(),
                tick_timer}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec = {Proc, {?MODULE, start_link, [Host, Opts]}, temporary, 1000, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).


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
init([Host, _Opts]) ->
    %% Gather gauge data at interval:
    TickTimer = timer:send_interval(?INTERVAL, tick),

    %% register_hooks to send events on the fly:
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, user_send_packet, 1),
    
    ?INFO_MSG("Started Timer: ~p", [TickTimer]),
    {ok, #state{host = Host, tick_timer = TickTimer}}.

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
handle_call(stop, _From, State) ->
    timer:cancel(State#state.tick_timer),
    {reply, ok, State};
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
handle_info(tick, #state{host = Host} = State) ->
    LHost = binary_to_list(Host),
    %%    ?INFO_MSG("Tick", []),
    OnlineUsers = ejabberd_sm:get_vh_session_number(Host),
    gauge(LHost, "users.online", OnlineUsers),
    %gauge("users.online." ++ LHost, OnlineUsers),
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

user_send_packet(Packet, _C2SState, _From, _To) ->
    incr("user_send_packet"),
    Packet.

gauge(Client, MetricName, GaugeValue) ->
    %?INFO_MSG("gauge = ~p:~p", [MetricName, GaugeValue]),
    send_data_to_grapherl(Client, MetricName, <<"g">>, GaugeValue).

incr(MetricName) ->
    incr(MetricName, 1).
incr(MetricName, Incr) ->
    ?INFO_MSG("incr = ~p:~p", [MetricName, Incr]).



send_data_to_grapherl(Client, MetricName, MetricType, Value) ->
    Data         = encode_data(Client, MetricName, MetricType, Value),
    {ok, Socket} = gen_udp:open(0),

    %% update the below to the correct location of the server
    GraperlServer = {127,0,0,1},

    gen_udp:send(Socket, GraperlServer, 11111, Data).


encode_data(Client, MetricName, Type, Val) when is_integer(Val) ->
    encode_data(Client, MetricName, Type, integer_to_binary(Val));
encode_data(Client, MetricName, Type, Val) when is_float(Val) ->
    encode_data(Client, MetricName, Type, float_to_binary(Val));

%% <<"www.server01.com/users.online:g/timestamp:value">>
encode_data(Client, MetricName, Type, Val) when is_binary(Val) ->
    TS          = erlang:integer_to_binary(unix_time()),
    MetricNameB = erlang:list_to_binary(MetricName),
    ClientB     = erlang:list_to_binary(Client),
    <<ClientB/binary, "/",
      MetricNameB/binary, ":", Type/binary, "/",
      TS/binary, ":", Val/binary>>.




unix_time() ->
    datetime_to_unix_time(erlang:universaltime()).

datetime_to_unix_time({{_,_,_},{_,_,_}} = DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200.
