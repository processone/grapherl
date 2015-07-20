%%%-------------------------------------------------------------------
%% @doc graph_web public API
%% @end
%%%-------------------------------------------------------------------

-module(graph_web_app).

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    %% taken from cowboy websocket tutorial
    Dispatch = cowboy_router:compile(
                 [{'_', [
                         {"/", cowboy_static, {priv_file, graph_web, "index.html"}},
                         {"/metric/data/:metric_name/:client_name/:range/:granularity", metric_handler, []},
                         {"/metric/list", metric_handler, []},
                         {"/static/[...]", cowboy_static, {priv_dir, graph_web, "static"}}
                        ]}
                 ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 9090}], [{env, [{dispatch, Dispatch}]}]),

    graph_web_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
