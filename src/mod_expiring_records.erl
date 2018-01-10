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
-export([add/3, fetch/1, delete/1, size/0, trim/0, search/1]).
-export([
    process_local_iq/1,
    process_local_iq_search/1,
    decode_iq_subel/1
]).
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
-define(NS_EXPIRING_RECORD_SEARCH, <<"urn:xmpp:expiring_record#search">>).

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
        IQDisc),
    gen_iq_handler:add_iq_handler(
        ejabberd_local,
        Host,
        ?NS_EXPIRING_RECORD_SEARCH,
        ?MODULE,
        process_local_iq_search,
        IQDisc).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_EXPIRING_RECORD_SEARCH),
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
                Now = erlang:system_time(seconds),
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

delete(Key) ->
    Trans = fun() ->
        Result = mnesia:match_object(expiring_records, #record{key = Key, value = '_', expires_at = '_'}, read),
        case Result of
            [{record, Key, _, _}] ->
                mnesia:delete(expiring_records, Key, write);
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
        Now = erlang:system_time(seconds),
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

search(KeyMatch) ->
    Trans = fun() ->
        mnesia:match_object(expiring_records, #record{key = KeyMatch, value = '_', expires_at = '_'}, read)
    end,
    {atomic, Result} = mnesia:transaction(Trans),
    Result.

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

-spec process_local_iq(iq()) -> iq().
process_local_iq(#iq{type=set, lang=Lang, sub_els=[Elem]} = IQ) ->
    ?INFO_MSG("process_local_iq set", []),
    {LJID, RoomJid, Action} = extract_attributes(Elem),
    case mod_muc:find_online_room(RoomJid#jid.user, RoomJid#jid.server) of
        {ok, Pid} ->
            case mod_muc_room:is_owner_or_admin(Pid, IQ#iq.from) of
                true ->
                    Category = proplists:get_value(<<"category">>, Elem#xmlel.attrs),
                    Key = {LJID, RoomJid#jid.user, RoomJid#jid.server, Category},
                    case Action of
                        <<"set">> ->
                            DurationAsString = proplists:get_value(<<"duration">>, Elem#xmlel.attrs),
                            {Duration, _} = string:to_integer(binary_to_list(DurationAsString)),
                            ExpiresAt = erlang:system_time(seconds) + Duration,
                            ?INFO_MSG("Registering temporary ~s: ~p", [Category, Key]),
                            add(Key, ok, ExpiresAt),
                            xmpp:make_iq_result(IQ);
                        <<"delete">> ->
                            ?INFO_MSG("Deleting temporary ~s: ~p", [Category, Key]),
                            delete(Key),
                            xmpp:make_iq_result(IQ)
                    end;
                false ->
                    Txt = <<"Must be admin or owner">>,
                    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang))
            end;
        error ->
            Txt = <<"Room not found">>,
            xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang))
    end;
process_local_iq(#iq{type=get, lang=Lang, sub_els=[Elem]} = IQ) ->
    Room = proplists:get_value(<<"room">>, Elem#xmlel.attrs),
    RoomJid = jid:from_string(Room),
    case mod_muc:find_online_room(RoomJid#jid.user, RoomJid#jid.server) of
        {ok, Pid} ->
            case mod_muc_room:is_owner_or_admin(Pid, IQ#iq.from) of
                true ->
                    Who = proplists:get_value(<<"jid">>, Elem#xmlel.attrs),
                    LJID = jid:tolower(jid:from_string(Who)),
                    Category = proplists:get_value(<<"category">>, Elem#xmlel.attrs),
                    Key = {LJID, RoomJid#jid.user, RoomJid#jid.server, Category},
                    case fetch(Key) of
                        not_found ->
                            Txt = <<"No entry">>,
                            xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang));
                        _ ->
                            ?INFO_MSG("~s has temporary ~s in ~s", [Who, Category, RoomJid#jid.user]),
                            xmpp:make_iq_result(IQ, #xmlel{name=Category})
                    end;
                false ->
                    Txt = <<"Must be admin or owner">>,
                    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang))
            end;
        error ->
            Txt = <<"Room not found">>,
            xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang))
    end.

-spec process_local_iq_search(iq()) -> iq().
process_local_iq_search(#iq{type=get, sub_els=[Elem]} = IQ) ->
    Room = proplists:get_value(<<"room">>, Elem#xmlel.attrs),
    RoomJid = jid:from_string(Room),
    Category = proplists:get_value(<<"category">>, Elem#xmlel.attrs),
    KeyMatch = {'_', RoomJid#jid.user, RoomJid#jid.server, Category},
    Items = search(KeyMatch),
    SearchResultsAsXmlel = search_results_to_xmlel(Items),
    xmpp:make_iq_result(IQ, #xmlel{name = <<"items">>, children=SearchResultsAsXmlel}).

extract_attributes(Elem) ->
    Who = proplists:get_value(<<"jid">>, Elem#xmlel.attrs),
    LJID = jid:tolower(jid:from_string(Who)),
    Room = proplists:get_value(<<"room">>, Elem#xmlel.attrs),
    RoomJid = jid:from_string(Room),
    Action = proplists:get_value(<<"action">>, Elem#xmlel.attrs),
    {LJID, RoomJid, Action}.

-spec decode_iq_subel(xmpp_element() | xmlel()) -> xmpp_element() | xmlel().
%% Tell gen_iq_handler not to auto-decode IQ payload
decode_iq_subel(El) ->
    El.

search_results_to_xmlel([]) ->
    [];

search_results_to_xmlel([Head|Tail]) ->
    [item_to_xmlel(Head)|search_results_to_xmlel(Tail)].

item_to_xmlel(#record{key={JID, Room, Host, Category}, expires_at=ExpiresAt}) ->
    Attrs = [
        {<<"jid">>, jid:to_string(JID)},
        {<<"room">>, jid:to_string(jid:make(Room, Host))},
        {<<"category">>, Category},
        {<<"expires_at">>, integer_to_binary(ExpiresAt)}
    ],
    #xmlel{name = <<"item">>, attrs = Attrs}.
