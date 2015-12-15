-module(eps_app).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

%%%===================================================================
start(_StartType, _StartArgs) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", eps_toppage_handler, []},
            {"/ws", eps_ws_handler, []}
		]}
	]),
    Port =
    case os:getenv("PORT") of
        false ->
            9090;
        P ->
            list_to_integer(P)
    end,
    %todo wss
    cowboy:start_http(http, 100, [{port, Port}], [
            {env, [{dispatch, Dispatch}]}
    ]).

stop(_State) ->
    ok.

%%%===================================================================
