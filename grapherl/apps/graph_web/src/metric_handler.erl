%%% Requires Cowboy 1.0

-module(metric_handler).

-export([init/2
        ,terminate/3
        ,known_methods/2
        ,allowed_methods/2
        ,is_authorized/2
        ,content_types_provided/2
        ,to_json/2
        ,resource_exists/2
        ,accept_resource/2
        ,delete_resource/2
        ]).


init(Req, _Opts) ->
    case cowboy_req:path(Req) of
        <<"/metric/list">> ->
            {cowboy_rest, Req, #{qtype => get, query => list}};

        <<"/metric/data/", _Rest/binary>> ->
            Metric       = cowboy_req:binding(metric_name, Req),
            Client       = cowboy_req:binding(client_name, Req),
            Range        = cowboy_req:binding(range, Req),
            {ok, Range1} = process_range(Range),
            Granularity  = cowboy_req:binding(granularity, Req, sec),
            QueryArgs    = #{metric => Metric,
                             client => Client,
                             range  => Range1,
                             granularity => Granularity},

            {cowboy_rest, Req, #{qtype      => get,
                                 query      => metric_data,
                                 query_args => QueryArgs}}

    end.


%% Perform any necessary cleanup of the state.
terminate(_Reason, _Req, _Opts) ->
    ok.


%% Return the list of known methods.
known_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>], Req, State}.


%% Return the list of allowed methods.
allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.


%% Return whether the user is authorized to perform the action.
is_authorized(Req, State) ->
    {true, Req, State}.


%% Return the list of content-types the resource provides.
content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.
    %{[{{<<"text">>, <<"html">>, '*'}, to_html}], Req, State}.

to_json(Req, #{data := Data} = State) ->
    Reply = erlang:iolist_to_binary(mochijson2:encode(Data)),
    {Reply, Req, State}.


%% Return whether the resource exists.
resource_exists(Req, #{qtype := get,
                       query := list} = State) ->
    {ok, List} = query_handler:get_metric_list(),
    {true, Req, State#{data => [{metric_list, List}]}};

resource_exists(Req, #{qtype      := get,
                       query      := metric_data,
                       query_args := QueryArgs} = State) ->
    #{metric      := Metric
     ,client      := Client
     ,range       := Range
     ,granularity := Granularity} = QueryArgs,

    {ok, List} = query_handler:get_metric_data(Metric, Client, Range,
                                               Granularity),
    {true, Req, State#{data => [{metric_data, List}]}}.


%% Process the request body.
%% No default value.
accept_resource(Req, State) ->
    {true, Req, State}.

%% Delete the resource.
delete_resource(Req, State) ->
    {false, Req, State}.



%% =============================================================================
%% internal funtions
%% =============================================================================

process_range(Range) ->
    case binary:split(Range, [<<":">>], [global]) of
        [Start, End] when Start >= End ->
            {ok, {Start, End}};
        [End, Start] when Start >= End ->
            {ok, {Start, End}}
    end.


