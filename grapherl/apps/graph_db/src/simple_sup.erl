-module(simple_sup).

-behaviour(supervisor).

%% API functions
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("apps/grapherl/include/grapherl.hrl").

-define(SERVER, ?MODULE).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Name, Type, ChildSpecs) ->
    ?INFO("Simple supervisor starting with args: ~p, ~p~n", [Name, Type]),
    supervisor:start_link({local, Name}, ?MODULE, [Type, ChildSpecs]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([Type, ChildSpecs]) ->
    ?INFO("~p starting with child specs: ~p~n~n", [?SERVER, ChildSpecs]),
    {ok, {{Type, 5, 10}, ChildSpecs}}.
    %{ok, {{simple_one_for_one, 5, 10}, [CHILD]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
