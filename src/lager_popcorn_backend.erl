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

-record(state, {level         :: atom(),
                popcorn_host  :: string(),
                popcorn_port  :: number()
}).

init(Params) ->
    Level        = config_val(level, Params, debug),
    Popcorn_Host = config_val(popcorn_host, Params, "localhost"),
    Popcorn_Port = config_val(popcorn_port, Params, 9125),

    {ok, #state{level        = Level,
                popcorn_host = Popcorn_Host,
                popcorn_port = Popcorn_Port}}.

handle_call({set_loglevel, Level}, State) ->
    {ok, ok, State#state{level=lager_util:level_to_num(Level)}};

handle_call(get_loglevel, State) ->
    {ok, State#state.level, State};

handle_call(_Request, State) ->
    {ok, not_implemented, State}.

handle_event({log, Dest, Level, {Date, Time}, Message}, State) ->
    io:format("Log message: ~p\n", [Message]),
    {ok, undefined};

handle_event(_Event, State) ->
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%
%% get a value from the application config
config_val(C, Params, Default) ->
    case lists:keyfind(C, 1, Params) of
        {C, V} -> V;
        _      -> Default
    end.

