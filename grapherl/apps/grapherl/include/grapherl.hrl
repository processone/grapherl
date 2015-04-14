%% Grapherl definitions
%%
%%

-include_lib("apps/grapherl/include/log.hrl").

%% default port receiving metric data
-define(R_PORT, 11111).
-define(ROUTER, r).

%% default port receiving requests
-define(S_PORT, 11112).
-define(DSRV, s).

-define(REQ_HANDLER_SUP, graph_db_server_sup).
-define(ROUTER_SUP, graph_db_router_sup).
-define(ROUTER_WORKER_SUP, graph_db_router_worker_sup).

-define(CHILD(Id, Mod, Args, Restart, Type), {Id, {Mod, start_link, Args},
                                              Restart, 5000, Type, [Mod]}).
