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

-define(SIMPLE_CHID(WorkerMod), ?CHILD(WorkerMod, WorkerMod, [], transient, worker)).

-define(MANAGER_CHID(WorkerMod), ?CHILD(WorkerMod, WorkerMod, [], transient, worker)).

-define(SIMPLE_SUP(SupId, WorkerMod),
        ?CHILD(SupId, simple_sup,
               [simple_one_for_one, [?SIMPLE_CHID(WorkerMod)]], permanent,
               supervisor )).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Process hirerarcy description 
%%====================================================================
%% router_worker : processes that handle the open UDP socket and receive  metric.
%% router_manager: gen_server process to (dynamicaly) spawn new graph_db_router
%% process. Also stores data regarding open UDP socket is case there are more
%% than one receiving sockets open.
%% 
%% graph_data_server: server that receives requests for gathering data. It
%% spawns a graph_db_aggregator process and starts listening for any new
%% requests.
%% 
%% graph_db_manager: stores meta data regarding ets cache and correspoding
%% database for different metrics, this data is stored in ETS table so that it
%% can be read concurrently by graph_db_worker processes so as correctly store
%% the data point in the table. It also stores user configration data eg.
%% TIME_INTERVAL after which cache should be dumped into database. It is
%% further resposible to implement these user configrations eg. dumping the
%% database after timeout.
%%
%% graph_db_worker: is a gen_server that caches and stores data into database.
%% User defines modules which should be used for caching and storing data,
%% default being mod_ETS and mod_levelDB. Both these modules are based on
%% gen_db behaviour module that defines necessary callbacks to implement
%% database modules.
%% We use poolboy lib to manage and perform action using graph_db_worker as the
%% worker process.
%% NOTE: dumping cache should be a tanscation actions during which no further
%% data is allowed to enter into cache. In order to achieve this we mark the
%% state (which would be stored in and ETS table) of cache db as unavailable
%% during the transaction. If any worker is in process of storing data into
%% cache we wait for it to terminate. 
%% NOTE: The ets table for storing state of cache table will have
%% {write_concurrency, false} which will prevent any process from reading the
%% state while it is being changed.
%%
%%                                                  +----------------+
%%                                                  | graph_db_sup   |
%%                                                  +--------+-------+
%%                                                           | (one_for_one)
%%                            +------------------------------+-------------------------------+
%%                            |                                                              |
%%                    +-------+----------+                                           +-------+-----+
%%                    |    ?DB_SUP       |                                           | ?ROUTER_SUP |
%%                    +------------------+                                           +-------+-----+
%%                            | (one_for_one)                                                | (one_for_all)            
%%              +-------------+------------+                                   +-------------+------------+             
%%              |                          |                                   |                          |             
%%   +----------+--------+         +-------+-----------+            +----------+-----------+      +-------+-----------+ 
%%   | graph_data_server |         | graph_db_manager  |            |   router_manager     |      | ?ROUTER_WORKER_SUP| 
%%   +-------------------+         +-------+-----------+            +----------------------+      +-------+-----------+ 
%%                                                                                                        |(simple_one_for_one)
%%                                                                                                  +-----|----------+  
%%                                                                                                +-------|---------+|  
%%                                                                                               +--------+--------+|+  
%%                                                                                               | graph_db_router |+   
%%                                                                                               +-----------------+    

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ?INFO("~p(~p): Starting~n", [?SERVER, []]),

    %% router for handling incoming metric data points
    RouterSupSpec =[?MANAGER_CHID(router_manager),
                    ?SIMPLE_SUP(?ROUTER_WORKER_SUP, graph_db_router)],

    %% TODO initalize poolboy with graph_db_worker

    DataServerSpec = ?MANAGER_CHID(graph_data_server),
    DbManagerSpec  = ?MANAGER_CHID(graph_db_manager),

    DbSupSpec = [DataServerSpec, DbManagerSpec ],

    %% TODO get database module from application environment.


    %% TCP server to listen for metric requests from Graph_map
    %Req_server_spec = ?CHILD(graph_db_server, graph_db_router, [], transient, worker),
    {ok, { {one_for_all, 500, 60},
           [
            ?CHILD(?ROUTER_SUP, simple_sup, [one_for_all, RouterSupSpec],
                   permanent, supervisor),
            ?CHILD(?DB_SUP, simple_sup, [one_for_one, DbSupSpec], permanent,
                   supervisor)
           ]
         }
    }.

%%====================================================================
%% Internal functions
%%====================================================================
