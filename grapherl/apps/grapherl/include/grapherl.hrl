%% Grapherl definitions
%%
%%

-include_lib("log.hrl").

%% default port receiving metric data
-define(R_PORT, 11111).
-define(ROUTER, r).

%% default port receiving requests
-define(S_PORT, 11112).
-define(DSRV, s).

-define(REQ_HANDLER_SUP, graph_db_server_sup).
-define(ROUTER_SUP, graph_db_router_sup).
-define(ROUTER_WORKER_SUP, graph_db_router_worker_sup).

-define(DATA_AGGR_SUP, graph_db_data_aggregator_sup).
-define(DB_SUP, db_sup).
-define(GEN_DB_WORKER_SUP, gen_db_worker_sup).
-define(DB_POOL, db_pool).

-define(ETS_SUP, ets_sup).

-define(CHILD(Id, Mod, Args, Restart, Type), {Id, {Mod, start_link, Args},
                                              Restart, 60000, Type, [Mod]}).

-record(struct, {data=[]}).
-record(packet, {mn, cn, mt, mp}).
