-module(testing).

-export([spawn_clients/2
        ,unix_time/0
        ]).


data(Client, Ts) ->
    Val  = erlang:integer_to_list(random:uniform(100)),
    TsS  = erlang:integer_to_list(Ts),
    Data = "{\"mid\": {\"cn\": \"" ++ Client ++ "\", \"mn\": \"cpu_usage\"}, \"mp\": {\"" ++ TsS ++ "\":" ++ Val ++ "}}",
    Data.



client(_Name, Socket, 0, _Ts) ->
    gen_udp:close(Socket);
client(Name, Socket, Num, Ts) ->
    Data = data(Name, Ts),
    io:format("[+] Sending ~p~n", [Data]),
    gen_udp:send(Socket, {127,0,0,1}, 11111, Data),
    ets:insert(testrouter, {key,val}),
    timer:sleep(5),
    client(Name, Socket, Num -1, Ts + 1).



spawn_clients(Num, Packets) ->
    spawn_clients(Num, 12000, Packets).

spawn_clients(0, _Port, _Packets) ->
    ok;
spawn_clients(Num, Port, Packets) ->
    erlang:spawn(
      fun() ->
              {ok, Socket} = gen_udp:open(Port),
              Name = "www.server" ++ erlang:integer_to_list(Port) ++ ".com",
              TS   = unix_time(),
              client(Name, Socket, Packets, TS)
      end),
    spawn_clients(Num -1, Port +1, Packets).




unix_time() ->
    datetime_to_unix_time(erlang:universaltime()).

datetime_to_unix_time({{_,_,_},{_,_,_}} = DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200.
