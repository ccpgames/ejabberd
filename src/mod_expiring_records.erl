%%%-------------------------------------------------------------------
%%% @author Snorri Sturluson <snorri.sturluson@ccpgames.com>
%%% @copyright (C) 2017, CCP hf
%%% @doc
%%%
%%% @end
%%% Created : 19. Oct 2017 10:24
%%%-------------------------------------------------------------------
-module(mod_expiring_records).
-author("snorri.sturluson@ccpgames.com").


-behavior(gen_mod).

%% API
-export([start/2, stop/1, reload/3, depends/2]).
-export([add/3, fetch/1, size/0, trim/0, process_local_iq/1, decode_iq_subel/1]).
-export([mod_opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").

-record(record, {
    key,
    value,
    expires_at
}).

-define(NS_EXPIRING_RECORD, <<"urn:xmpp:expiring_record">>).

start(Host, Opts) ->
    prepare_table(),
    ?INFO_MSG("Registering handler for expiring_record on ~s", [Host]),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, gen_iq_handler:iqdisc(Host)),
    gen_iq_handler:add_iq_handler(
        ejabberd_local,
        Host,
        ?NS_EXPIRING_RECORD,
        ?MODULE,
        process_local_iq,
        IQDisc).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_EXPIRING_RECORD),
    ok.

reload(_Host, _NewOpts, _OldOpts) ->
    erlang:error(not_implemented).

depends(_Host, _Opts) ->
    [].

mod_opt_type(_) ->
    [].

add(Key, Value, ExpiresAt) ->
    Trans = fun() ->
        Record = #record{key=Key, value=Value, expires_at = ExpiresAt},
        mnesia:write(expiring_records, Record, write)
    end,
    {atomic, ok} = mnesia:transaction(Trans),
    ok.

fetch(Key) ->
    ?DEBUG("expiring_records:fetch(~p)", [Key]),
    Trans = fun() ->
        Result = mnesia:match_object(
            expiring_records,
            #record{key = Key, value = '_', expires_at = '_'},
            read),
        case Result of
            [{record, Key, Value, ExpiresAt}] ->
                Now = erlang:system_time(second),
                case Now < ExpiresAt of
                    true ->
                        {ok, Value};
                    _ ->
                        mnesia:delete(expiring_records, Key, write),
                        not_found
                end;
            [] ->
                not_found
        end
    end,
    {atomic, Result} = mnesia:transaction(Trans),
    Result.

size() ->
    mnesia:table_info(expiring_records, size).

trim() ->
    Trans = fun() ->
        Now = erlang:system_time(second),
        MatchHead = #record{key='$1', expires_at = '$2', _='_'},
        Guard = {'>', Now, '$2'},
        Result = '$1',
        ExpiredKeys = mnesia:select(
            expiring_records,
            [{MatchHead, [Guard], [Result]}]),
        delete_records(ExpiredKeys)
    end,
    {atomic, ok} = mnesia:transaction(Trans),
    ok.

prepare_table() ->
    DbNodes = mnesia:system_info(db_nodes),
    case catch mnesia:table_info(expiring_records, attributes) of
        {'EXIT', _} ->
            %% Table does not exist - create it
            ?DEBUG("Creating expiring records table", []),
            {atomic, ok} = mnesia:create_table(
                expiring_records, [
                    {attributes, record_info(fields, record)},
                    {record_name, record},
                    {disc_copies, DbNodes}
                ]
            ),
            ok;
        _Attributes ->
            ?DEBUG("expiring records table already exists", []),
            ok
    end,
    mnesia:wait_for_tables([expiring_records], infinite).

delete_records([]) ->
    ok;

delete_records([Head | Tail]) ->
    mnesia:delete(expiring_records, Head, write),
    delete_records(Tail).

process_local_iq(#iq{type=set, lang=Lang, sub_els=[Elem]} = IQ) ->
    Who = proplists:get_value(<<"jid">>, Elem#xmlel.attrs),
    LJID = jid:tolower(jid:from_string(Who)),
    Room = proplists:get_value(<<"room">>, Elem#xmlel.attrs),
    RoomJid = jid:from_string(Room),
    Action = proplists:get_value(<<"action">>, Elem#xmlel.attrs),
    DurationAsString = proplists:get_value(<<"duration">>, Elem#xmlel.attrs),
    {Duration, _} = string:to_integer(DurationAsString),
    case mod_muc:find_online_room(RoomJid#jid.user, RoomJid#jid.server) of
        {ok, Pid} ->
            case mod_muc_room:is_owner_or_admin(Pid, IQ#iq.from) of
                true ->
                    ExpiresAt = erlang:system_time(seconds) + Duration,
                    Key = {LJID, RoomJid#jid.user, RoomJid#jid.server, Action},
                    ?INFO_MSG("Registering temporary ~s for ~p in ~p", [Action, Who, Room]),
                    add(Key, ok, ExpiresAt),
                    xmpp:make_iq_result(IQ);
                false ->
                    Txt = <<"Must be admin or owner">>,
                    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang))
            end;
        error ->
            Txt = <<"Room not found">>,
            xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang))
    end.

-spec decode_iq_subel(xmpp_element() | xmlel()) -> xmpp_element() | xmlel().
%% Tell gen_iq_handler not to auto-decode IQ payload
decode_iq_subel(El) ->
    El.