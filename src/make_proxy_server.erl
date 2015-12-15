-module(make_proxy_server).
-export([start/0]).

start() ->
    application:ensure_all_started(make_proxy_server).

