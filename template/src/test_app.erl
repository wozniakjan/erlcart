-module(test_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Routes       = routes(),
    Dispatch     = cowboy_router:compile(Routes),
    Port         = port(), 
    TransOpts    = [{port, Port}, {ip, ip()}],
    ProtoOpts    = [{env, [{dispatch, Dispatch}]}],
    NumAcceptors = 100,
    {ok, _}      = cowboy:start_http(http, NumAcceptors, TransOpts, ProtoOpts),
    test_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

routes() ->
    [
        {'_', [
                {"/", index_handler, []},
                {"/[...]", error_pages, []}
            ]
        }
    ].

port() ->
    {ok, Port} = application:get_env(http_port),
    Port.

ip() ->
    {ok, IP} = inet_parse:address(os:getenv("OPENSHIFT_ERL_IP")),
    IP.
