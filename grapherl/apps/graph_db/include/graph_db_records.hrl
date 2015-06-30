%% records for handling the state of db_workers

%% db_mod: stores the name of module to be used for secondary storage.
%% cache_mod: stores the name of module to be user for ram storage.
%% db_configs: list of lists, where each element lists stores configs related
%% to database storing metric.
-record(state, {db_mod, cache_mod, db_configs}).

-define(SEC, sec).
-define(MIN, min).
-define(HOUR, hour).
-define(DAY, day).

-define(MSG_QUEUE, msg_queue).

