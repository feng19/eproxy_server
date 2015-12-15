-module(mp_child).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {key, lsock, socket, remote}).

-include("socks_type.hrl").
-include("debug.hrl").

-define(TIMEOUT, 1000 * 60 * 10).

%%%===================================================================
start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).


%%%===================================================================
init([LSock]) ->
    {ok, Key} = application:get_env(make_proxy_server, key),
    {ok, #state{key=Key, lsock = LSock}, 0}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, #state{lsock = LSock, socket = undefined} = State) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    ?DEBUG(Socket),
    mp_sup:start_child(),
    {noreply, State#state{socket=Socket}, ?TIMEOUT};

%% send by OPT timeout
handle_info(timeout, #state{socket = Socket} = State) when is_port(Socket) ->
    {stop, timeout, State};

%% first message from client
handle_info({tcp, Socket, Request}, #state{key = Key, socket = Socket, remote = undefined} = State) ->
    ?DEBUG({Socket,Request}),
    case connect_to_remote(Request, Key) of
        {ok, Remote} ->
            ok = inet:setopts(Socket, [{active, once}]),
            ok = inet:setopts(Remote, [{active, once}]),
            {noreply, State#state{remote=Remote}, ?TIMEOUT};
        {error, Error} ->
            ?DEBUG(Error),
            {stop, Error, State}
    end;

%% recv from client, and send to server
handle_info({tcp, Socket, Request}, #state{key=Key, socket=Socket, remote=Remote} = State) ->
    ?DEBUG(Request),
    {ok, RealData} = mp_crypto:decrypt(Key, Request),
    ?DEBUG(RealData),
    case gen_tcp:send(Remote, RealData) of
        ok ->
            ok = inet:setopts(Socket, [{active, once}]),
            {noreply, State, ?TIMEOUT};
        {error, Error} ->
            ?DEBUG(Error),
            {stop, Error, State}
    end;

%% recv from server, and send back to client
handle_info({tcp, Socket, Response}, #state{key=Key, socket=Client, remote=Socket} = State) ->
    ?DEBUG(Response),
    case gen_tcp:send(Client, mp_crypto:encrypt(Key, Response)) of
        ok ->
            ok = inet:setopts(Socket, [{active, once}]),
            {noreply, State, ?TIMEOUT};
        {error, Error} ->
            ?DEBUG(Error),
            {stop, Error, State}
    end;

handle_info({tcp_closed, _}, State) ->
    {stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State}.


terminate(_Reason, #state{socket=Socket, remote=Remote}) ->
    case is_port(Socket) of
        true -> gen_tcp:close(Socket);
        false -> ok
    end,

    case is_port(Remote) of
        true -> gen_tcp:close(Remote);
        false -> ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================

-spec connect_to_remote(binary(), nonempty_string()) -> {ok, {list()|tuple(), non_neg_integer()}} |
                                                      {error, term()}.
connect_to_remote(Data, Key) ->
    case mp_crypto:decrypt(Key, Data) of
        {ok, RealData} ->
            <<AType:8, Rest/binary>> = RealData,
            {ok, {Address, Port}} = parse_address(AType, Rest),
            connect_target(Address, Port);
        {error, Error} ->
            {error, Error}
    end.


-spec parse_address(non_neg_integer(), binary()) -> {ok, {list()|tuple(), non_neg_integer()}}.
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


-spec connect_target(list() | tuple(), non_neg_integer()) -> {ok, port()} |
                                                             {error, term()}.
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

