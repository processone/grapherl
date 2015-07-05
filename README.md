# grapherl
ejabberd monitoring server

# Quick start
Clone this repo to your system:

      $ git clone https://github.com/processone/grapherl.git

Compile and run

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

    {num_routers, 3}
Router processes receive incoming UDP traffic. This configration specifies the number of processes which will monitor
the socket and receive incoming data. The current configration can handle around 1 million points per minute. It should
be noted that mindlessly increasing the number of processes monitoring the socket can degrade performance.

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



