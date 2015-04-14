-module(graph_db_socket).


%% API functions
-export([handle_req/2,
         set_sockopt/2,
        init_inet_async/1]).

-include_lib("apps/grapherl/include/grapherl.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================

handle_req(?ROUTER, Socket) ->
    case gen_udp:recv(Socket, 4) of
        {error, Reason} ->
            ?ERROR("Socket Worker(~p): ~p~n", [router, Reason]),
            {ok};
        {ok, {_Address,_Port, Packet}}->
            {ok, Packet}
    end;
handle_req(?DSRV, {ListenSock, CliSocket}) ->
    case set_sockopt(ListenSock, CliSocket) of
        ok              -> ok;
        {error, Reason} -> exit({set_sockopt, Reason})
    end,

    %% Signal the network driver to accept another connection
    {ok, NewRef} = init_inet_async(ListenSock),
    {ok, NewRef, CliSocket}.

set_sockopt(ListSock, CliSocket) ->
    true = inet_db:register_socket(CliSocket, inet_tcp),
    case prim_inet:getopts(ListSock, [active, nodelay, keepalive, delay_send, priority, tos]) of
        {ok, Opts} ->
            case prim_inet:setopts(CliSocket, Opts) of
                ok    -> ok;
                Error -> gen_tcp:close(CliSocket), Error
            end;
        Error ->
            gen_tcp:close(CliSocket), Error
    end.

init_inet_async(Socket) ->
    case prim_inet:async_accept(Socket, -1) of
        {ok,    NewRef} -> {ok, NewRef};
        {error, NewRef} -> exit({async_accept, inet:format_error(NewRef)})
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
