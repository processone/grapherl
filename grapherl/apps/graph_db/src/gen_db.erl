-module(gen_db).

%% behaviour module for implementing custom caching/storage modules
%% 

-type data()      :: binary().
-type dataL()     :: [data()].
-type db_name()   :: any().
-type ret_status():: atom().
-type arg()       :: {any(), any()}.
-type args()      :: [arg()].

-callback init_db(db_name(), args()) -> {ok, ret_status()}.

-callback delete_db(db_name()) -> {ok, ret_status()}.

-callback insert(db_name(), data()) -> {ok, ret_status()}.

-callback insert_many(db_name(), dataL()) -> {ok, ret_status()}.

-callback read_all(db_name()) -> {ok, dataL()}.

-callback delete_all(db_name()) -> {ok, ret_status()}.
