#Architecture for GraphErl

##graph_web (Webapp)

###implementation essentials
- probably cowboy based webapp. the webapp will communicate to graph_map app on
some predefined port. This will allow users to swap the webapp with their
custom version of django/rails app that talks to graph_mp app on the predefined
port.
- Graph rquests should be cached so as to  
- maintaining a web app cache in important to serve graphs quickly.

##graph_map (Graph Matrix Aggregator Processor):
- modules: grap_ma.app.src, graph_ma_app.erl, grap_ma_sup.erl, graph_ma.erl 
- graph_map_handler: tcp server that listens for graph requests from the
webapp.
- graph_map: based on each incomming request for graph generation, a graph_map
process is spawned that aggregates data and processes it to generate graphs.

>NOTE: gen_map will not bring in data untill and unless it is essentially
>required. It will aggregate the location of metric and spwan a worker process
>on all the erlang nodes that contains the data. This worker will process data
>and return results to be interpreted as graphs. (NOT sure about this
        >implementation)

##graph_db
- modules: grap_db.app.src, graph_db_app.erl, grap_db_sup.erl,
    grap_db_router.erl, gen_cache.erl, gen_db.erl, grap_db.erl
- configuraion
    - allow user to specify an upper limit on number of data points cached.
    - time/data point interval after which cache is dumped into database.
- grap_db_server: gen_udp server that receives the data and casts it to the
corresponding databse api module (which is module using gen_db behaviour).
- gen_cache: behaviour module of implementing ram caching of incomming metirc
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

>NOTE: - there may be multiple instances of gen_db running on one or more
    nodes each caching and storing metric of one kind of data points. Futher,
    gen_db servers running on single node storing parts of same metric will
    cache the metric in the same ETS table i.e. for caching metric of one kind
    there will be a single ETS table.
> - The number of ETS tables per erlang node is limited to ~1500

### clustering graph_db app can be configured to spawn mulitple gen_db (and
        maybe grap_db_router) processes on different nodes for load
distribution, scaling and redundency

- implementation details:
- gproc is used for process registration of gen_db servers.
- each gen_db process will register under two labels:
- name :`{n, l, {Metric_Name, Client_Name}}`
- property : `{p, l, {Metric_Name}}`
- grap_db_router receives data points which it will forward to corresponding
gen_db servers using gproc:bcast/3 because there maybe multiple gen_db servers
running to store same or part of metric data for redundancy or load
distribution respectively.

>NOTE: gen_db servers storing the same metric on different nodes are assumed to
>have the same name

(NOTE : following data point format and its details are just a proposal not the
 final format any suggestions are welcome)

- data point format : "METRIC_PATH TimeStamp Value"
- METRIC_PATH may be of the format :
    - Metric_Name.Client_Name.[u|m|b]
    - alternate : `[{metric, Metric_Name}, {client, Client_Name}, {type,
        u|m|b}]`

    eg. cpu_usage.client1.u

    **u** : unicast, will send the data point to the correspoding gen_db
    process

    **m** : multicast, multiple gen_db servers may be storing parts of the
    metirc. so using "m" in the PATH will indicate the graph_db_router to
forward the data to particular gen_db based on certain rules.  (the is left for
        future extension)

    **b** : braodcast, forwards the received data point to all the gen_db
    process that are registered under the property `{p,l{Metric_Name}}`. this
    is to be used when user want to have redundancy.
