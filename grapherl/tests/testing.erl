-module(testing).

-export([spawn_clients/2]).


%% Num : specifies the number of differnt metrics to simulate
%% Packets : specifies the number of data points to be sent for each metric
%% usage: testing:spawn_clients(100, 1000) % 100 metrics, each metrics populated
%% with 1000 data points.
spawn_clients(Num, Packets) ->
    spawn_clients(Num, 12000, Packets).

spawn_clients(0, _Port, _Packets) ->
    ok;
spawn_clients(Num, Port, Packets) ->
    io:format("[+] Started data sending ~n", []),
    erlang:spawn(
      fun() ->
              {ok, Socket} = gen_udp:open(Port),
              %Name = "www.server" ++ erlang:integer_to_list(Port) ++ ".com",
              Name = "metric_" ++ erlang:integer_to_list(Port),
              TS   = unix_time(),
              client(Name, Socket, Packets, TS)
      end),
    spawn_clients(Num -1, Port +1, Packets).


client(_Name, Socket, 0, _Ts) ->
    gen_udp:close(Socket);
client(Name, Socket, Num, Ts) ->
    Data = data(Name, Ts),
    Ret  = gen_udp:send(Socket, {127,0,0,1}, 11111, Data),
    io:format("[+] Sending ~p ~p ~n", [Data, Ret]),
    timer:sleep(20),
    client(Name, Socket, Num -1, Ts + 60).


%% generate random data point
data(Name, Ts) ->
    Val  = erlang:integer_to_list(crypto:rand_uniform(1000000, 100000000)),
    TsS  = erlang:integer_to_list(Ts),
    %Data = Client ++ "/cpu_usage:g/" ++ TsS ++ ":" ++ Val, 
    Data = "www.site01.com/" ++ Name ++ ":g/" ++ TsS ++ ":" ++ Val, 
    Data.


unix_time() ->
    datetime_to_unix_time(erlang:universaltime()).

datetime_to_unix_time({{_,_,_},{_,_,_}} = DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200.
