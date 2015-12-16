-module(eproxy_server).
-export([
    start/0,
    debug/3,
    debug/4
]).

start() ->
    application:ensure_all_started(eproxy_server).

-ifdef(debug).

debug(Module,Line,Msg) ->
    io:format("[~p:~p] ~p~n",[Module,Line,Msg]).
debug(Module,Line,Format,Args) ->
    io:format("[~p:~p] "++Format++"~n",[Module,Line|Args]).

-else.

debug(_Module,_Line,_Msg) ->
    ok.
debug(_Module,_Line,_Format,_Args) ->
    ok.

-endif.