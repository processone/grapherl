# Grapherl
Real-time scalable monitoring server


# Images
![Grapherl](https://github.com/processone/grapherl/blob/feat-graph-web/grapherl/images/grapherl.png)



# Quick start
Clone this repo to your system:

      $ git clone https://github.com/processone/grapherl.git


**Prerequisites**:  Before executing `make` please make sure you have **Erlang/OTP 17.x** installed


Create directory for storing metric objects

      $ sudo mkdir -p /var/db/grapherl       


Compile and run

      $ cd grapherl
      $ make && sudo make console

Grapherl client is located at **localhost:9090**


**NOTE**:
If `make` fails then mail the error at `kansi13 at gmail dot com` with the subject `grapherl compile error`,
you will get a reply within couple of minutes.

**If you any question/issues regarding Grapherl you can also find me (kansi) at #erlang irc**


# Getting data into Grapherl
Grapherl by default listens on port `11111`. Format for sending a metric point is as follows:

      client_name/metric_name:metric_type/time_stamp:value

      randomClient1/memory_usage:g/1441005678:1002938389   # example


- **metric_name**: Specify the metric name for eg. cpu_usage, online_users, memory etc.
- **client_name**: metric_name along with client_name are used to uniquely identify a point. Client name can be anything for eg. `server01`,  `website01.com`,  `website 101`,   `1284398` etc. **NOTE** Grapherl considers client_name just as plane string meaning that there is **no** special interpretation for `website_101.com` as compared `website_101`, they are both just (different) strings for Grapherl.
- **metric_type**: there are 2 types allowed `g` (gauge) and `c` (counter). Metric types are discussed [here](https://github.com/etsy/statsd/blob/master/docs/metric_types.md). By default for a gauge metric values are averaged over and interval and for counter metric values are added over an interval.
- **time_stamp**: epoch time.
- **value**: value for the given time_stamp.

Sample python (`data_feed.py`) and erlang (`testing.erl`) modules which feed data into Grapherl located under the `grapherl/tests` directory.
You can also play around with Grapherl by feeding data using these modules.

If you have any queries regarding feeding data into Grapherl mail them at `kansi13 at gmail dot com`.


# Configrations
Grapherl consists of 2 components **graph_db** which receives UDP data and stores it, **graph_web** which retrives this
data and creates nice visualizations for the user.

## graph_db
**Brief description** :

Before we discuss the various configrations we give an overview of how this subapp works
so that the user can wisely configure these options. All incoming data is received by graph_db, muliple processes
(known as router_workers) wait on the socket to receive high amount of UDP traffic.

These received packets are forwarded to a process called db_worker (which is pool of worker processes) which
decodes this received packet and stores it in ram (inside ETS tables). All incoming points are aggreated into ram
and after timeout are written to disk.

Further grapherl expects huge amount of data, so storing such amount of data as is for long is not feasible. Hence,
grapherl constanly purges data according to a pre-defined scheme. To understand this scheme let consider that
a client (i.e. a server which send total number of online users each second). The purging scheme works as follows:

  - Points which are aggreated each 1 sec are kept as is for one day. So, we would have around 86400 points at the end of the day.
  - After one day these point are purged to a granularity of one minute i.e. all points in one minute are purged and value is averaged (or processed according to the metric type). Points at a granularity of minute are stored for a week.
  - After a week, points at granularity of minute are purged to granularity of hour and kept for a year.
  - After a year, points at granularity of hour are purged to day and kept indefinitely.


### configrations
The following configrations can be found in file `graph_db.app.src` located at `grapherl/apps/graph_db/src/graph_db.app.src`

    {storage_dir, <<"/var/db/grapherl/">>}
Specifies directory location where graph_db will store data points on disk. Note, user should make sure that
directory exits and should start grapherl with necessary permissions (i.e. root permissions in this case).

    {ports, [11111]}
Specifies ports on which graph_db will listen. User can specify muliple port for eg. `{ports, [11111, 11112]}`

    {num_routers, 3}
Router processes receive incoming UDP traffic. This configration specifies the number of processes which will monitor
**each** opened socket and receive incoming data. The current configration can handle around 1 million points per
minute. It should be noted that mindlessly increasing the number of processes monitoring the socket can degrade
performance.

    {cache_to_disk_timeout, 60000}
Specifies the timeout (in millisecond) after which the aggerated points stored in ram will dumped onto the disk.

    {db_daemon_timeout, 60000}
This options defines the timeout (in millisecond) after which data points stored (on disk) are checked for purging.

    {cache_mod, db_ets}
    {db_mod, db_levelDB}

**cache_mod** defines the module to be used to ram storage and **db_mod** defines the module to be used for
disk storage. By default graph_db uses ETS for ram storage and levelDB for disk storage but the user is not restricted
to using these defaults. Users can write their custom db modules, place them in the `src` directory of graph_db app.
The user must note that these modules are based on custom behaviour called gen_db (defined in graph_db). In order to write
custom module user can refer to the existing implementation or submit an issue to support the given db.

### How to optimize you graph_db configuration
Configuring graph_db according to the expected load is very crucial to achieve best performance. For eg. too much
router_worker processes can degrade performance, not having or having more number of db_worker than the hardware
can support will also degrade perfomance. Also cache_to_disk_timeout should be carefully decided in accordance with
the expected UDP traffic so that you don't run out of ram. Lastly, keeping db_daemon_timeout very low can lead to
unnecessary processing hence degrading performance.

So, we discuss some perfomance details of graph_db. NOTE this testing was done on second generation
Intel(R) Core(TM) i5-2430M CPU (4 processors). The grapherl directory contains a module named `testing.erl`,
which has been used to test graph_db. Following are some results:
  - with 3 router_workers monitoring one socket graph_db can handle around 1 million data points per minute beyond this data loss increases considerably.
  - these 1 million data points we gracefully handled buy 3 db_workers but its highly recommended that the user allow more db_workers as these processes are also responsible for other tasks too. So, when handling such high data its good have more db_workers so that load on each worker is less.
  - It must be duly noted by the user that more number of db_workers will directly increase the cpu usage. For eg. having 6 db_workers on a system that has only 4 processors is not advisible. The default number i.e. 3, works fine on a system with 4 processors.

  
### Handling huge number of data points
If you are someone who wants to go beyond receiving 1 million points per minute, Grapherl has something for you.
You don't need to spin up another grapherl instance for that, all you need to do is throw some more hardware at
Grapherl and tweak the configrations. Assuming you have bought more hardware, to handle more data it advisable
to receive data on multiple ports for eg. if you use 2 ports to receive data you can already receive 2 million
points per minute. Now, to handle these data points you will need to have more db workers (minimum 6). And since you are
going to increase db_workers make sure you have sufficient cpu threads (atleast 8 if you run 6 db_workers). 

**NOTE**: The configrations suggested in this section are mere speculation based on previously discussed testing results.
You can test Grapherl using the `testing.erl` module and while you are testing you can monitor the system using native
erlang app called `observer` which has been included in Grapherl.


### Handling large number of metrics
In case you want to track a lot of metrics graph_db allows the user to bootstrap ram and disk db objects
for metrics before any data starts coming in. Doing this will be helpful becasue creating ram and disk db objects is
a time consuming task, so while receiving such huge traffic it is advisible that the user bootstrap some of the metrics
so that the system doesn't fall under sudden load (though the app can handle sudden loads it just to assure constant cpu usage).
In order to bootstrap metric user needs have a file in the following format:

     cpu_usage, g
     user_count, c
     memory_usage, g
     system_load, g


each line contains the metric name and type seperated by comma. Once you have this file created, execute the following
in the grapherl (erlang) shell:

     db_manager:pre_process_metric("/absolute/path/to/metric/file")

NOTE: the above routine of boostraping metric is purely optional. This is be used in case you want to track a lot of metrics
and that too when you expect to receive a burst of new data points none of which has its corresponding metric objects created.


When tracking a large number of metrics it is advisible to increase the `ulimit`. For eg. if you are tracking like 500
different metrics then set ulimit to `ulimit -n 10000`.



### graph_db across multiple boots
graph_db maintains a list of metric names for which it is receiving data. This state is stored in a file name `db_manager.dat`
located in `storage_dir` directory. This is to ensure that even across multiple VM restarts or in case of a VM crash
graph_db knows which metrics objects it was receiving. So, if you want
to restart Grapherl but don't want it reload its previous state remove this file before restarting. On the other hand, if you
want to migrate Grapherl to some other server but want it to be at the same state where the current instance of Grapherl is
running then just take the `db_manager.dat` file and place it in `storage_dir`.

**Note**: the storage format for `db_manager.dat` is same as that of the bootstrap file discussed in the previous section.


## graph_web

**Brief description**:

This sub-app is responsible for serving data gathered by Grapherl. There isn't much to configure in graph_web except
the port at which the web server listens. The default port is 9090 but user is free to change it acc to their needs. But
remember to start Grapherl with necessary permissions (eg. sudo in case port < 1024).

Now we discuss various features offered on the client side. 

  - The client side has a global side-bar which shows active metrics. This global side-bar is auto-refreshed every 10 minutes but the user can manually refresh by clicking on the refresh button located in the top right corner of the global side-bar.
  - Clicking on any of the metrics in side-bar will start a display which will show the data aggregated in the past 3 hours.
  - A display is a frame/panel which displays the graphs. It is composed of two parts: toolbar and chart display area. There are two kinds of displays that can be created:
    - Single displays: these span the width.
    - Split displays : two displays places side-by-side.
  - User can add these displays by clicking the pink buttons located in the top-right corner (of global toolbar).
  - User can add/remove multiple metrics to these displays. To add a metric click on "+" button in display toolbar, this will activate metric selection mode and "square button" will appear in place of the "+" button. Select all the metrics from the global side-bar which you want to add to the display. Once you selected the desired metrics click the "square button" to exit out of the metric selection mode. Once you are out of the metric selection mode all selected metric will be added to the display.
  - User can specify the range, granularity at which the graphs are to be displayed.
  - To display live data you can click the live button and also configure the interval after which new data will be requested from the server.
  - User can change the display type for any metric by clicking the gear button in display toolbar. Currently 3 types of graphs are supported (bar, line, spline)
  - If the data is not available at the current selected granularity then it will serverd at either higher or lower granularity (which ever is available).
  - User can also add x/y grids line, additional y axis (for different scales), subgraph view or rotate axis. All these can be configured by clicking on the stacked bars icon in display toolbar.




**NOTE**:
Though Grapherl allows users to specify granularity at which they want to see data, graph_web serves data based on its
availability and not on queried granularity. What this means is that if user wants to retrieve data at a particular granularity,
Grapherl will try it best to serve at queried granularity. If the data is not available at the queried granularity then Grapherl
will serve data at higher or lower granularity depending on which ever is available. If data is higher granularity then it is
compressed to the queried granularity.


# Contributors
- Vanshdeep Singh [@kansi](https://github.com/kansi): Architect and developer.
- Mickaël Rémond [@mremond](https://github.com/mremond): active feedback and suggestions (mentor)
- Christophe Romain [@cromain](https://github.com/cromain): active feedback and suggestions.
