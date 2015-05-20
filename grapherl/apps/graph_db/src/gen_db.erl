-module(gen_db).

%% behaviour module for implementing custom caching/storage modules
%% 

-type data()      :: binary().
-type dataL()     :: [data()].
-type db_name()   :: any().
-type state()     :: any().

-callback init_db(db_name(), state()) -> {ok, state()}.

-callback delete_db(db_name(), state()) -> {ok, state()}.

-callback insert(db_name(), data(), state()) -> {ok, state()}.

-callback insert_many(db_name(), dataL(), state() ) -> {ok, state()}.

-callback read_all(db_name(), state()) -> {ok, dataL(), state()}.

-callback delete_all(db_name(), state()) -> {ok, state()}.
