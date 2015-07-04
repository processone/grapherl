-module(gen_db).

%% behaviour module for implementing custom caching/storage modules
%% 

-type data()      :: binary().
-type dataL()     :: [data()].
-type db_name()   :: any().
-type db_fd()     :: any().
-type client()    :: binary().
-type arg()       :: {any(), any()}.
-type args()      :: [arg()].

-callback init_db(db_name(), args())                -> {ok, db_fd()}.

-callback open_db(db_name(), args())                -> {ok, db_fd()}.

-callback close_db(db_fd())                         -> {ok, success}.

-callback delete_db(db_name())                      -> {ok, success}.

-callback insert(db_name(), client(), data())       -> {ok, success}.

-callback insert_many(db_name(), client(), dataL()) -> {ok, success}.

-callback read(db_fd(), client())                   -> {ok, dataL()}.

-callback read_all(db_fd())                         -> {ok, dataL()}.

-callback delete_many(db_fd(), client(), dataL())   -> {ok, success}.

-callback clear_client(db_fd(), client())           -> {ok, success}.
