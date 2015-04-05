#Architecture for Grapherl

##Terminology
- **Lattice**   : refers to set of points collected for a patricular activity
eg. cpu usage lattice.
- **datapoint** : refers to `{key, value}` pair belonging to particular lattice
where key is generally timestamp.
- **precision** : referes to the minimum difference between 2 given consecutive
points. eg. precision of 1 sec.

##graph_web (Webapp)

###implementation essentials
- probably cowboy based webapp which will communicate with graph_map on
a predefined port. This will allow users to swap the webapp with their
custom version of django/rails app that talks to graph_mp app on that port.
- on client side d3.js maybe used to generate graphs (need to explore more
options to compare and contrast).
- client requesting for a graph must specify the granularity of the lattice it
wants to receive along with specifying the lattice name.

##graph_map (Grapherl Matrix Aggregater Processor):
- modules: graph_map.app.src, graph_map_app.erl, graph_map_sup.erl, graph_map_router.erl,
graph_map.erl 
- graph_map_router: tcp server that listens for lattice requests from the webapp
and routes it to corresponding graph_map gen_server.
- graph_map: is a gen_server process that spawns processes on one or more nodes
which aggregate the requested lattice. The lattice collected is cached for furhter
requests. A graph_map process will be spawned on the first incoming request for
a particular lattice.

##graph_db (Grapherl DataBase)
- modules: graph_db.app.src, graph_db_app.erl, graph_db_sup.erl,
graph_db_router.erl, gen_cache.erl, gen_db.erl, graph_db.erl
- configuration
  - allow user to specify an upper limit on number of data points cached.
  - time/data point interval after which cache is dumped into database.
- grap_db_router: gen_udp server that receives the data and casts it to the
corresponding databse api module (which is module using gen_db behaviour).
- gen_cache: behaviour module of implementing ram caching of incoming lattice
points.
- graph_ETS: gen_cache module for caching using ETS.
- gen_db : this is a behaviour module based on gen_server. It specifies
database api functions as callbacks. Internally it caches received data into an
ETS table and on timeout dumps the table into database. ETS caching is required
becuase we would receive enormous amount of data points every minute, since
disk I/O can be slow we need to cache these points and write them in chunks.
Since it based of the gen_server user can store details of their database
inside "State" which will passed as default argument to each callback.
Furhtermore, any request for data retrival by the webapp will served by a
gen_db module which will return latest data by combining ETS cache and data
from database itself, hence providing the latest data

- graph_levelDB: gen_db module that stores data into the levelDB.

>NOTE:
> - there may be multiple instances of gen_db running on one or more
>  nodes each caching and storing lattice of one kind of data points. Futher,
>  gen_db servers running on single node storing parts of same lattice will
>  cache the lattice in the same ETS table i.e. for caching lattice of one kind
>  there will be a single ETS table.
> - The number of ETS tables per erlang node is limited to ~1500

- clustering graph_db app can be configured to spawn mulitple gen_db (and maybe
grap_db_router) processes on different nodes for load distribution, scaling and
redundency.

- implementation details:
        - gproc is used for process registration of gen_db servers.
        - each gen_db process will register under two labels:
        - name :`{n, l, {lattice_Name, Client_Name}}`
        - property : `{p, l, {lattice_Name}}`

- grap_db_router receives data points which it will forward to corresponding
gen_db servers using gproc:bcast/3 because there maybe multiple gen_db servers
running to store same or part of lattice data for redundancy or load
distribution respectively.

>NOTE: gen_db servers storing the same lattice on different nodes are assumed to
>have the same name

## ejabberd module for stats aggregation
Following data point format and its details are just a proposal not the
final format. Any suggestions are welcomed.

- data point format : [{lid, LatticeId}, {lp, {TimeStamp, Value}}]
    - LatticeId (lid) may be of the format : `[{ln, lattice_name}, {cn, client_name}, {t, u|m|b}]`
    - acronyms :
        - lp : lattice point
        - ln : lattice name
        - cn : client name
        - t  : type (specifies the forwarding strategy for the lattice point)

    eg. `[{lid, [{ln, cpu_usage}, {cn, www.server01.com}, {t,u}]}, {lp, [{TimeStamp, 47}]}]`

    **u** : unicast, will send the data point to the corresponding gen_db
    process

    **m** : multicast, multiple gen_db servers may be storing parts of the
    metirc. so using ***m*** in the PATH will indicate the graph_db_router to
    forward the data to particular gen_db based on certain rules.  (the is left for
    future extension)

    **b** : braodcast, forwards the received data point to all the gen_db
    process that are registered under the property `{p,l {lattice_name}}`. This
    is to be used when user want to have redundancy.
