# grapherl
ejabberd monitoring server

# Quick start
Clone this repo to your system:

      $ git clone https://github.com/processone/grapherl.git

Compile and run

      $ cd grapherl
      $ make && make console


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
a client (i.e. a server send the number of online users each second). The purging scheme works as follows:

  - Points which are aggreated each 1 sec are kept as is for one day. So, we would have around 86400 points at the end of the day.
  - After one day these point are purged to a granularity of one minute i.e. all points in one minute are purged and value is averaged (or processed according to the type of metric). Points at a granularity of minute are stored for a week.
  - After a week points at granularity of minute are purged to granularity of hour.
  - After a year points at granularity of hour are purged to day and kept as indefinitely.


### configrations

    {storage_dir, <<"/var/db/grapherl/">>}
Specifies directory location where graph_db will store data points on disk. Note, user should make sure that
directory exits and should start grapherl with necessary permissions (i.e. root permissions in this case).

    {ports, [11111]}
Specified the ports on which graph_db will listen. User can specify muliple port for eg. `{ports, [11111, 11112]}`

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
to using these defaults. Users can write their custom db modules place them in the src directory of graph_db app.
The user must note that these modules are based on custom behaviour moudle called gen_db. In order to write
custom module user can reffer to the existing implementation or submit an issue to support the given db.

### How to optimize you graph_db configuration
Configuring graph_db according to the expected load is very crucial is achieve best performance. For eg. too much
router_worker processes can degrade performance, not having or having more number of db_worker than you hardware
can support will also degrade perfomance. Also cache_to_disk_timeout should be carefully decided in accordance with
the expected UDP traffic so that you don't run out of ram. Lastly, keeping db_daemon_timeout very low can lead
unnecessary processing hence degrading performance.

So, we discuss some perfomance details of graph_db. NOTE this testing was done on second generation
Intel(R) Core(TM) i5-2430M CPU (4 processors). The grapherl directory contains a module named `testing.erl`,
which has been used to test graph_db. Following are some results:
  - with 3 router_workers monitoring one socket graph_db can handle around 1 million data points per minute.
  - these 1 million data points we gracefully handled buy 3 db_workers but it highly recommended that the user allow more db_workers as these processes are also responsible for other tasks too. So, when handling such high data its good have more db_workers so that load on each worker is less.
  - It must be duly noted by the user that more no. of db_workers will directly increase the cpu usage. For eg. having 6 db_workers on a system that has only 4 processors is not advisible. The default number i.e. 3, works fine on a system with 4 processors.

  
### Handling huge number of data points
If user wants to handle more than 1 million data points per minute it is advisable to use receive data on multiple
ports. Along with increasing number of ports user will also need the number of graph_db workers too. As previously
discussed number of db_workers should be increased in accordance with the available hardware.

Also, in case when user wants to track a lot of metrics graph_db allows the user to bootstrap ram and disk db objects
for metrics before any data starts coming in. Since creation ram and disk db objects is time consuming bootstraping
will help reduce the load.




         
