-module(tcp_server).
-behaviour(gen_server).
-export([start_link/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-record(state, {
    listener,
    clients = []
}).

start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

stop() ->
    gen_server:cast(?MODULE, stop).

init([Port]) ->
    process_flag(trap_exit, true),
    
    case gen_tcp:listen(Port, [
        binary,
        {packet, 4},
        {reuseaddr, true},
        {active, true},
        {backlog, 100}
    ]) of
        {ok, ListenSocket} ->
            io:format("TCP Server started on port ~p~n", [Port]),
            self() ! accept,
            {ok, #state{listener = ListenSocket}};
        {error, Reason} ->
            io:format("Failed to start TCP server: ~p~n", [Reason]),
            {stop, Reason}
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(accept, State = #state{listener = ListenSocket}) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            Pid = spawn_link(fun() -> handle_client(Socket) end),
            gen_tcp:controlling_process(Socket, Pid),
            NewClients = [Pid | State#state.clients],
            {noreply, State#state{clients = NewClients}};
        {error, closed} ->
            {stop, normal, State};
        {error, Reason} ->
            io:format("Accept error: ~p~n", [Reason]),
            {noreply, State}
    end,
    self() ! accept,
    {noreply, State};

handle_info({'EXIT', Pid, _Reason}, State) ->
    NewClients = lists:delete(Pid, State#state.clients),
    {noreply, State#state{clients = NewClients}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{listener = ListenSocket}) ->
    gen_tcp:close(ListenSocket),
    io:format("TCP Server stopped~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_client(Socket) ->
    receive
        {tcp, Socket, Data} ->
            try
                JsonData = jsx:decode(Data, [return_maps]),
                process_client_message(Socket, JsonData)
            catch
                _:Error ->
                    io:format("JSON decode error: ~p~n", [Error]),
                    send_error(Socket, "Invalid JSON format")
            end,
            handle_client(Socket);
        {tcp_closed, Socket} ->
            io:format("Client disconnected~n"),
            ok;
        {tcp_error, Socket, Reason} ->
            io:format("TCP error: ~p~n", [Reason]),
            ok
    end.

process_client_message(Socket, #{<<"type">> := <<"register">>, 
                                 <<"user_id">> := UserId,
                                 <<"username">> := Username}) ->
    case messenger_server:register_user(UserId, Username) of
        {ok, registered} ->
            Response = #{type => <<"register_response">>,
                        status => <<"success">>,
                        message => <<"User registered successfully">>},
            send_json(Socket, Response);
        {error, user_exists} ->
            send_error(Socket, "User already exists")
    end;

process_client_message(Socket, #{<<"type">> := <<"message">>,
                                 <<"from">> := From,
                                 <<"to">> := To,
                                 <<"content">> := Content}) ->
    case messenger_server:send_message(From, To, <<"text">>, Content) of
        {ok, sent} ->
            Response = #{type => <<"message_response">>,
                        status => <<"success">>},
            send_json(Socket, Response);
        {error, Reason} ->
            send_error(Socket, atom_to_binary(Reason, utf8))
    end;

process_client_message(Socket, #{<<"type">> := <<"get_users">>}) ->
    case messenger_server:get_online_users() of
        {ok, Users} ->
            Response = #{type => <<"users_list">>,
                        users => Users},
            send_json(Socket, Response);
        {error, Reason} ->
            send_error(Socket, atom_to_binary(Reason, utf8))
    end;

process_client_message(Socket, _) ->
    send_error(Socket, "Unknown message type").

send_json(Socket, Data) ->
    Json = jsx:encode(Data),
    gen_tcp:send(Socket, Json).

send_error(Socket, Message) ->
    Response = #{type => <<"error">>,
                message => list_to_binary(Message)},
    send_json(Socket, Response).
