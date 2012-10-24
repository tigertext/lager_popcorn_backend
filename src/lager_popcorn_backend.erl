-module(lager_popcorn_backend).
-author('marc.e.campbell@gmail.com').
-behaviour(gen_event).

-export([init/1,
         handle_call/2,
         handle_event/2,
         handle_info/2,
         terminate/2,
         code_change/3
]).

-record(state, {socket,
                level         :: atom(),
                popcorn_host  :: string(),
                popcorn_port  :: number(),
                node_role     :: string(),
                node_version  :: string()
}).

init(Params) ->
    Level        = proplists:get_value(level, Params, debug),
    Popcorn_Host = proplists:get_value(popcorn_host, Params, "localhost"),
    Popcorn_Port = proplists:get_value(popcorn_port, Params, 9125),
    Node_Role    = proplists:get_value(node_role, Params, "no_role"),
    Node_Version = proplists:get_value(node_version, Params, "no_version"),

    {ok, Socket} = gen_udp:open(0, [list]),

    {ok, #state{socket       = Socket,
                level        = Level,
                popcorn_host = Popcorn_Host,
                popcorn_port = Popcorn_Port,
                node_role    = Node_Role,
                node_version = Node_Version}}.

handle_call({set_loglevel, Level}, State) ->
    {ok, ok, State#state{level=lager_util:level_to_num(Level)}};

handle_call(get_loglevel, State) ->
    {ok, State#state.level, State};

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_event({log, {lager_msg, _, _, Severity, {Date, Time}, Message}}, State) ->
    Encoded_Message = encode_protobuffs_message(node(), State#state.node_role, State#state.node_version, Severity, Date, Time, Message),

    gen_udp:send(State#state.socket,
                 State#state.popcorn_host,
                 State#state.popcorn_port,
                 Encoded_Message),

    {ok, State};

handle_event(_Event, State) ->
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    %% TODO version number should be read here, or else we don't support upgrades
    {ok, State}.

encode_protobuffs_message(Node, Node_Role, Node_Version, Severity, _Date, _Time, Message) ->
    erlang:iolist_to_binary([
        protobuffs:encode(1, atom_to_list(Node), string),
        protobuffs:encode(2, Node_Role, string),
        protobuffs:encode(3, Node_Version, string),
        protobuffs:encode(4, lager_util:level_to_num(Severity), uint32),
        protobuffs:encode(5, Message, string)
    ]).


