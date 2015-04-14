%%%-------------------------------------------------------------------
%% @doc graph_db top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(graph_db_sup).
-author('kansi13@gmail.com').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("apps/grapherl/include/grapherl.hrl").

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ?INFO("~p(~p): Starting~n", [?SERVER, []]),

    %% router for handling incoming metric data points
    RouterWorker_spec   = ?CHILD(graph_db_router, graph_db_router, [],
                                 transient, worker),

    RouterManagerSpec   = ?CHILD(graph_router_manager, socket_manager,
                                 [{type, ?ROUTER}], permanent, worker),

    RouterWorkerSupSpec = ?CHILD(?ROUTER_WORKER_SUP, simple_sup,
                                 [simple_one_for_one, [RouterWorker_spec]],
                                 permanent, supervisor),

    RouterSupChildSpec  = [ RouterWorkerSupSpec, RouterManagerSpec ],

    %% TCP server to listen for metric requests from Graph_map
    %Req_server_spec = ?CHILD(graph_db_server, graph_db_router, [], transient, worker),
    {ok, { {one_for_all, 500, 60},
           [
            ?CHILD(?ROUTER_SUP, simple_sup, [one_for_all, RouterSupChildSpec],
                   permanent, supervisor)
            %?CHILD(?REQ_HANDLER_SUP, simple_sup, [one_for_one, Req_server_spec],
                   %permanent, supervisor),
           ]
         }
    }.

%%====================================================================
%% Internal functions
%%====================================================================
