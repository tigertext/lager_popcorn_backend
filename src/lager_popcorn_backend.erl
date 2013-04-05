-module(lager_popcorn_backend).
-author('marc.e.campbell@gmail.com').
-behaviour(gen_event).

-export([init/1,
         handle_call/2,
         handle_event/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         get_app_version/0
]).

-record(state, {socket            :: pid(),
                lager_level_type  :: 'mask' | 'number' | 'unknown',
                level             :: atom(),
                popcorn_host      :: string(),
                popcorn_port      :: number(),
                node_role         :: string(),
                node_version      :: string()
}).

init(Params) ->
    %% we need the lager version, but we aren't loaded, so... let's try real hard
    %% this is obviously too fragile
    {ok, Properties}     = application:get_all_key(),
    {vsn, Lager_Version} = proplists:lookup(vsn, Properties),
    Lager_Level_Type =
      case string:to_float(Lager_Version) of
          {V1, _} when V1 < 2.0 -> 'number';
          {V2, _} when V2 =:= 2.0 -> 'mask';
          {_, _} -> 'unknown'
      end,

    Level        = lager_util:level_to_num(proplists:get_value(level, Params, debug)),
    Popcorn_Host = proplists:get_value(popcorn_host, Params, "localhost"),
    Popcorn_Port = proplists:get_value(popcorn_port, Params, 9125),
    Node_Role    = proplists:get_value(node_role, Params, "no_role"),
    Node_Version = proplists:get_value(node_version, Params, "no_version"),

    {ok, Socket} = gen_udp:open(0, [list]),

    {ok, #state{socket            = Socket,
                lager_level_type  = Lager_Level_Type,
                level             = Level,
                popcorn_host      = Popcorn_Host,
                popcorn_port      = Popcorn_Port,
                node_role         = Node_Role,
                node_version      = Node_Version}}.

handle_call({set_loglevel, Level}, State) ->
    {ok, ok, State#state{level=lager_util:level_to_num(Level)}};

handle_call(get_loglevel, State) ->
    {ok, State#state.level, State};

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_event({log, {lager_msg, Q, Metadata, Severity, {Date, Time}, _, Message}}, State) ->
    handle_event({log, {lager_msg, Q, Metadata, Severity, {Date, Time}, Message}}, State);

handle_event({log, {lager_msg, _, Metadata, Severity, {Date, Time}, Message}}, #state{level=L}=State) ->
    case lager_util:level_to_num(Severity) =< L of
        true ->
            Module = proplists:get_value(module, Metadata),
            Function = proplists:get_value(function, Metadata),
            Line = proplists:get_value(line, Metadata),
            Pid = proplists:get_value(pid, Metadata),
            Encoded_Message = encode_protobuffs_message(State#state.lager_level_type,
                                                        node(), State#state.node_role, State#state.node_version, Severity, Date,
                                                        Time, Message, Module, Function, Line, Pid),
            gen_udp:send(State#state.socket,
                         State#state.popcorn_host,
                         State#state.popcorn_port,
                         Encoded_Message);
         _ -> ok
    end,
    {ok, State};

handle_event(_Event, State) ->
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    %% TODO version number should be read here, or else we don't support upgrades
    Vsn = get_app_version(),
    {ok, State#state{node_version=Vsn}}.

encode_protobuffs_message('unknown', Node, Node_Role, Node_Version, Severity, _Date, _Time, Message, Module, Function, Line, Pid) ->
    encode_protobuffs_message('mask', Node, Node_Role, Node_Version, Severity, _Date, _Time, Message, Module, Function, Line, Pid);
encode_protobuffs_message('number', Node, Node_Role, Node_Version, Severity, _Date, _Time, Message, Module, Function, Line, Pid) ->
    %% convert the number to a mask
    Mask = lager_severity_to_mask(Severity),
    encode_protobuffs_message('number', Node, Node_Role, Node_Version, Mask, _Date, _Time, Message, Module, Function, Line, Pid);
encode_protobuffs_message('mask', Node, Node_Role, Node_Version, Severity, _Date, _Time, Message, Module, Function, Line, Pid) ->
    erlang:iolist_to_binary([
        protobuffs:encode(1, 1, uint32),   %% Packet version
        protobuffs:encode(2, atom_to_list(Node), string),
        protobuffs:encode(3, Node_Role, string),
        protobuffs:encode(4, Node_Version, string),
        protobuffs:encode(5, lager_util:level_to_num(Severity), uint32),
        protobuffs:encode(6, Message, string),
        protobuffs:encode(7, opt(Module, <<"">>), string),
        protobuffs:encode(8, opt(Function, <<"">>), string),
        protobuffs:encode(9, opt(Line, <<"">>), string),
        protobuffs:encode(10, opt(Pid, <<"">>), string)
    ]).

%% Return the protobufs data for optional fields
opt(undefined, Default) -> Default;
opt(Value, _) when is_integer(Value) -> list_to_binary(integer_to_list(Value));
opt(Value, _) when is_pid(Value)     -> list_to_binary(pid_to_list(Value));
opt(Value, _) when is_atom(Value)    -> list_to_binary(atom_to_list(Value));
opt(Value, _) when is_binary(Value)  -> Value;
opt(Value, _) when is_list(Value)    -> list_to_binary(Value);
opt(_, Default) -> Default.

get_app_version() ->
    [App,Host] = string:tokens(atom_to_list(node()), "@"),
    Apps = application:which_applications(),
    Vsn = case proplists:lookup(list_to_atom(App), Apps) of
        none -> "no_version";
        {_, _, V} -> V
    end.

lager_severity_to_mask(debug)     -> 128;
lager_severity_to_mask(info)      -> 64;
lager_severity_to_mask(notice)    -> 32;
lager_severity_to_mask(warn)      -> 16;
lager_severity_to_mask(error)     -> 8;
lager_severity_to_mask(critical)  -> 4;
lager_severity_to_mask(alert)     -> 2;
lager_severity_to_mask(emergency) -> 1;
lager_severity_to_mask(none)      -> 0.

