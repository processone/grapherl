-module(graph_utils).

-export([get_args/2,
         open_port/2,
         get_socket/2,
         decode_packet/1]).

-include_lib("grapherl.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================

%% TL : TupleList
%% get specified params in ParamList from TL
get_args(TL, ParamList) ->
    try get_args0(TL, ParamList, []) of
        Params -> {ok, Params}
    catch
        error:Error -> {error_params_missing, Error}
    end.

%% helper func for get_router_args/2
get_args0(_TL, [], Acc) ->
    lists:reverse(Acc);
get_args0(TL, [Key|T], Acc) ->
    {Key, Val} = lists:keyfind(Key, 1, TL),
    get_args0(TL, T, [Val| Acc]).

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

%% decode joson
%% Mid : MetricId 
%% Mn  : Metric Name
%% Cn  : Client Name eg. www.server01.com
%% Mp  : Metric Point eg [{timestamp, value}]
decode_packet(Packet) when is_binary(Packet) ->
    PacketD = mochijson2:decode(Packet),
    {ok, [Mid, Mp]}  = get_args(PacketD#struct.data, [<<"mid">>, <<"mp">>]),
    {ok, [Mn, Cn]}   = get_args(Mid#struct.data, [<<"mn">>, <<"cn">>]),
    [{Key, Value}]   = Mp#struct.data,
    #packet{mn=Mn, cn=Cn, mp={Key, Value}}.
