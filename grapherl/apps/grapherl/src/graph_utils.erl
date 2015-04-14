-module(graph_utils).

-export([get_router_args/2,
         open_port/2,
         get_socket/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% TL : TupleList
%% get specified params in ParamList from TL
get_router_args(TL, ParamList) ->
    try get_router_args0(TL, ParamList, []) of
        Params -> {ok, Params}
    catch
        error:Error -> {error, params_missing, Error}
    end.

%% helper func for get_router_args/2
get_router_args0(_TL, [], Acc) ->
    lists:reverse(Acc);
get_router_args0(TL, [Key|T], Acc) ->
    {Key, Val} = lists:keyfind(Key, 1, TL),
    get_router_args0(TL, T, [Val| Acc]).

open_port(udp, Port) ->
    gen_udp:open(Port, [{active, false}, binary]);
open_port(tcp, Port) ->
    gen_tcp:listen(Port, [binary, {packet, 4}, {active, false},
                          {reuseaddr, true}] ).

get_socket(_Type, []) ->
    {error, false};
get_socket(Type, [Sock | L]) ->
    case lists:keyfind(type,1,Sock) of
        {type, Type} ->
            {socket, Socket} = lists:keyfind(socket, 1, Sock),
            {ok, Socket};
        _ ->
            get_socket(Type, L)
    end.
