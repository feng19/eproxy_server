-module(mp_ws_handler).

-behaviour(cowboy_websocket_handler).

-export([
    init/3,
    websocket_init/3,
    websocket_handle/3,
    websocket_info/3,
    websocket_terminate/3
]).

-include("socks_type.hrl").
-include("debug.hrl").

-define(TIMEOUT, 1000 * 60 * 10).

-record(state, {key, remote}).

%%===================================================================

init({tcp, http}, _Req, _Opts) ->
    ?DEBUG("ws init"),
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    ?DEBUG("ws init 2"),
    {ok, Key} = application:get_env(make_proxy_server, key),
	{ok, Req, #state{key=Key}, ?TIMEOUT}.

%% first message from client
websocket_handle({binary, Msg}, Req, #state{key = Key, remote = undefined} = State) ->
    ?DEBUG(Msg),
    case connect_to_remote(Msg, Key) of
        {ok, Remote} ->
            ok = inet:setopts(Remote, [{active, once}]),
            {ok, Req, State#state{remote=Remote}};
        {error, Error} ->
            ?DEBUG(Error),
            {shutdown, Req, State}
    end;
%% 	{reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};

%% recv from client, and send to server
websocket_handle({binary, Msg}, Req, #state{key=Key, remote=Remote} = State) ->
    ?DEBUG(Msg),
    {ok, RealData} = mp_crypto:decrypt(Key, Msg),
    ?DEBUG(RealData),
    case gen_tcp:send(Remote, RealData) of
        ok ->
            {ok, Req, State};
        {error, Error} ->
            ?DEBUG(Error),
            {shutdown, Req, State}
    end;

websocket_handle(Data, Req, State) ->
    ?DEBUG("unknow msg:~p",[Data]),
	{ok, Req, State}.

%% send by OPT timeout
websocket_info(timeout, Req, State) ->
    ?DEBUG("timeout shutdown"),
    {shutdown, Req, State};

%% recv from server, and send back to client
websocket_info({tcp, Socket, Response}, Req, #state{key=Key, remote=Socket} = State) ->
    ?DEBUG(Response),
    Msg = mp_crypto:encrypt(Key, Response),
    ok = inet:setopts(Socket, [{active, once}]),
    {reply, {binary,Msg}, Req, State};

websocket_info({tcp_closed, _}, Req, State) ->
    {shutdown, Req, State};

websocket_info({tcp_error, _, Reason}, Req, State) ->
    ?DEBUG("tcp_error:~p",[Reason]),
    {shutdown, Req, State};

websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, #state{remote=Remote}) ->
    case is_port(Remote) of
        true ->
            gen_tcp:close(Remote);
        false ->
            ok
    end,
	ok.

%%===================================================================

connect_to_remote(Data, Key) ->
    case mp_crypto:decrypt(Key, Data) of
        {ok, RealData} ->
            <<AType:8, Rest/binary>> = RealData,
            {ok, {Address, Port}} = parse_address(AType, Rest),
            connect_target(Address, Port);
        {error, Error} ->
            {error, Error}
    end.

parse_address(?IPV4, Data) ->
    <<Port:16, Destination/binary>> = Data,
    Address = list_to_tuple( binary_to_list(Destination) ),
    {ok, {Address, Port}};

parse_address(?IPV6, Data) ->
    <<Port:16, Destination/binary>> = Data,
    Address = list_to_tuple( binary_to_list(Destination) ),
    {ok, {Address, Port}};


parse_address(?DOMAIN, Data) ->
    <<Port:16, _DomainLen:8, Rest/binary>> = Data,
    Address = binary_to_list(Rest),
    {ok, {Address, Port}}.


connect_target(Address, Port) ->
    connect_target(Address, Port, 2).

connect_target(_, _, 0) ->
    {error, connect_failure};

connect_target(Address, Port, RetryTimes) ->
    case gen_tcp:connect(Address, Port, [binary, {active, false}], 5000) of
        {ok, TargetSocket} ->
            {ok, TargetSocket};
        {error, _Error} ->
            connect_target(Address, Port, RetryTimes-1)
    end.