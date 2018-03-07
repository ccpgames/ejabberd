%%%-------------------------------------------------------------------
%%% @author snorri
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Nov 2017 10:03
%%%-------------------------------------------------------------------
-module(mod_associations).
-author("snorri.sturluson@ccpgames.com").

%% API
-export([]).

-behavior(gen_mod).

%% API
-export([start/2, stop/1, reload/3, depends/2]).
-export([
    process_local_iq/1,
    process_local_iq_eve_user_data/1,
    process_purge_session_channels/1,
    decode_iq_subel/1
]).
-export([mod_opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").

-define(NS_ASSOCIATIONS, <<"urn:xmpp:associations">>).
-define(NS_EVE_USER_DATA, <<"urn:xmpp:eve_user_data">>).
-define(NS_MUC_PURGE_SESSION_CHANNELS, <<"urn:xmpp:purge_session_channels">>).

% Associations are used to determine access to rooms, to allow membership
% based on EVE corporations or alliances.
% User data is extra data that is sent along with presence notifications
% so that EVE clients don't have to query the EVE server for this information.
% The user data may include corp and alliance info, but we keep it separate
% even if it means duplication as it allows us to have the user data be a
% black box to Ejabberd - it merely passes the info along.

start(Host, Opts) ->
    ?INFO_MSG("Registering handler for associations on ~s", [Host]),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, gen_iq_handler:iqdisc(Host)),
    gen_iq_handler:add_iq_handler(
        ejabberd_local,
        Host,
        ?NS_EVE_USER_DATA,
        ?MODULE,
        process_local_iq_eve_user_data,
        IQDisc),
    gen_iq_handler:add_iq_handler(
        ejabberd_local,
        Host,
        ?NS_ASSOCIATIONS,
        ?MODULE,
        process_local_iq,
        IQDisc),
    gen_iq_handler:add_iq_handler(
        ejabberd_local,
        Host,
        ?NS_MUC_PURGE_SESSION_CHANNELS,
        ?MODULE,
        process_purge_session_channels,
        IQDisc).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_MUC_PURGE_SESSION_CHANNELS),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_ASSOCIATIONS),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_EVE_USER_DATA),
    ok.

reload(_Host, _NewOpts, _OldOpts) ->
    erlang:error(not_implemented).

depends(_Host, _Opts) ->
    [].

mod_opt_type(_) ->
    [].

-spec process_local_iq_eve_user_data(iq()) -> iq().
process_local_iq_eve_user_data(#iq{type=set, from=From, lang=Lang, sub_els=[Elem]} = IQ) ->
    case From of
        #jid{luser = <<"admin">>} ->
            ?INFO_MSG("process_local_iq_eve_user_data set", []),
            Who = proplists:get_value(<<"jid">>, Elem#xmlel.attrs),
            Key = {eve_user_data, Who},
            ExpiresAt = erlang:system_time(seconds) + 24*60*60,
            [Data] = Elem#xmlel.children,
            mod_expiring_records:add(Key, Data#xmlel.attrs, ExpiresAt),
            ?INFO_MSG("Set EVE user data for ~p to ~p", [Who, Data#xmlel.attrs]),
            xmpp:make_iq_result(IQ);
        _ ->
            Txt = <<"Only admin can set user data">>,
            xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang))

    end;
process_local_iq_eve_user_data(#iq{type=get, sub_els=[Elem]} = IQ) ->
    ?INFO_MSG("process_local_iq_eve_user_data get", []),
    Who = proplists:get_value(<<"jid">>, Elem#xmlel.attrs),
    Key = {eve_user_data, Who},
    case mod_expiring_records:fetch(Key) of
        {ok, Data} ->
            ?INFO_MSG("EVE user data for ~s: ~p", [Who, Data]);
        _ ->
            ?INFO_MSG("No EVE user data found for ~s", [Who])
    end,
    xmpp:make_iq_result(IQ).

-spec process_local_iq(iq()) -> iq().
process_local_iq(#iq{type=set, lang=Lang, from=From, sub_els=[Elem]} = IQ) ->
    case From of
        #jid{luser = <<"admin">>} ->
            {UserJid, CorpJid, AllianceJid} = extract_attributes(Elem),
            % Association to last for one week. On regular servers 24 hrs
            % is enough, but local servers often run longer.
            % TODO: Make this a config value
            ExpiresAt = erlang:system_time(seconds) + 7*24*60*60,
            ?INFO_MSG("Registering association for user ~p: ~p", [UserJid, {CorpJid, AllianceJid}]),
            mod_expiring_records:add({association, UserJid}, {CorpJid, AllianceJid}, ExpiresAt),
            mod_expiring_records:add({is_association, CorpJid}, ok, ExpiresAt),
            mod_expiring_records:add({is_association, AllianceJid}, ok, ExpiresAt),
            xmpp:make_iq_result(IQ);
        _ ->
            Txt = <<"Only admin can set associations">>,
            xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang))

    end;
process_local_iq(#iq{type=get, lang=Lang, from=From, sub_els=[Elem]} = IQ) ->
    case From of
        #jid{luser = <<"admin">>} ->
            Who = proplists:get_value(<<"jid">>, Elem#xmlel.attrs),
            UserJid = jid:tolower(jid:from_string(Who)),
            case mod_expiring_records:fetch({association, UserJid}) of
                not_found ->
                    Txt = <<"No entry">>,
                    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang));
                {Corp, Alliance} ->
                    ?INFO_MSG("~p is associated with ~p and ~p", [Corp, Alliance]),
                    Attrs = [
                        {<<"corp">>, jid:tolower(jid:to_string(Corp))},
                        {<<"alliance">>, jid:tolower(jid:to_string(Alliance))}
                    ],
                    xmpp:make_iq_result(IQ, #xmlel{name = <<"association">>, attrs = Attrs })
            end;
        _ ->
            Txt = <<"Only admin can get associations">>,
            xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang))

    end.

-spec process_purge_session_channels(iq()) -> iq().
process_purge_session_channels(#iq{type = get, lang = Lang} = IQ) ->
    Txt = <<"Value 'get' of 'type' attribute is not allowed">>,
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_purge_session_channels(#iq{from = From, to = To, type = set, lang = Lang} = IQ) ->
    Host = To#jid.lserver,
    ServerHost = ejabberd_router:host_of_route(Host),
    AccessAdmin = gen_mod:get_module_opt(ServerHost, mod_muc, access_admin, none),
    case acl:match_rule(ServerHost, AccessAdmin, From) of
        allow ->
            ?INFO_MSG("process_purge_session_channels", []),
            mod_muc:purge_session_rooms(Host),
            xmpp:make_iq_result(IQ);
        _ ->
            ?INFO_MSG("process_purge_session_channels only allowed by admin", []),
            Txt = <<"Only admin can purge session channels">>,
            xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang))
    end.

extract_attributes(Elem) ->
    Who = proplists:get_value(<<"jid">>, Elem#xmlel.attrs),
    UserJid = jid:tolower(jid:from_string(Who)),
    Corp = proplists:get_value(<<"corp">>, Elem#xmlel.attrs),
    CorpJid = jid:tolower(jid:from_string(Corp)),
    Alliance = proplists:get_value(<<"alliance">>, Elem#xmlel.attrs),
    AllianceJid = jid:tolower(jid:from_string(Alliance)),
    {UserJid, CorpJid, AllianceJid}.

-spec decode_iq_subel(xmpp_element() | xmlel()) -> xmpp_element() | xmlel().
%% Tell gen_iq_handler not to auto-decode IQ payload
decode_iq_subel(El) ->
    El.
