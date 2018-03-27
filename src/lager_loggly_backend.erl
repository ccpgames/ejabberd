%%%-------------------------------------------------------------------
%%% @author snorri
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Mar 2018 14:38
%%%-------------------------------------------------------------------
-module(lager_loggly_backend).
-author("snorri.sturluson@ccpgames.com").

-behaviour(gen_event).

-export([
    init/1,
    handle_call/2,
    handle_event/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    level :: integer(),
    loggly_url :: string()
}).

-include_lib("lager/include/lager.hrl").

init([Level, LogglyUrl]) ->
    State = #state{
        level = lager_util:level_to_num(Level),
        loggly_url = LogglyUrl
    },
    ?INT_LOG(info, "Starting Loggly lager backend with ~s", [LogglyUrl]),
    {ok, State}.

handle_event({log, Message}, #state{level=Level} = State) ->
    case lager_util:is_loggable(Message, Level, ?MODULE) of
        true ->
            Proplist = metadata_to_binary_proplist(
                lager_msg:metadata(Message), [
                    {<<"level">>, logutils:any_to_binary(lager_msg:severity(Message))},
                    {<<"message">>, logutils:any_to_binary(lager_msg:message(Message))}
                ]),
            Payload = logutils:proplist_to_json(Proplist),
            Request = {State#state.loggly_url, [{"te", "chunked"}], "application/json", Payload},
            httpc:request(post, Request, [], [{body_format, binary}]);
        _ ->
            ok
    end,
    {ok, State};

handle_event(_Event, State) ->
    {ok, State}.

handle_call(get_loglevel, #state{level = Level} = State) ->
    {ok, Level, State};
handle_call({set_loglevel, Level}, State) ->
    {ok, ok, State#state{level = lager_util:level_to_num(Level)}};
handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


metadata_to_binary_proplist(Metadata, Proplist) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            [{logutils:any_to_binary(Key), logutils:any_to_binary(Value)} | Acc]
        end,
        Proplist,
        Metadata
    ).

