%%%-------------------------------------------------------------------
%%% @author snorri
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Nov 2017 10:03
%%%-------------------------------------------------------------------
-module(mod_associations).
-author("snorri").

%% API
-export([]).

-behavior(gen_mod).

%% API
-export([start/2, stop/1, reload/3, depends/2]).
-export([process_local_iq/1, decode_iq_subel/1]).
-export([mod_opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").

-define(NS_ASSOCIATIONS, <<"urn:xmpp:associations">>).

start(Host, Opts) ->
    ?INFO_MSG("Registering handler for associations on ~s", [Host]),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, gen_iq_handler:iqdisc(Host)),
    gen_iq_handler:add_iq_handler(
        ejabberd_local,
        Host,
        ?NS_ASSOCIATIONS,
        ?MODULE,
        process_local_iq,
        IQDisc).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_ASSOCIATIONS),
    ok.

reload(_Host, _NewOpts, _OldOpts) ->
    erlang:error(not_implemented).

depends(_Host, _Opts) ->
    [].

mod_opt_type(_) ->
    [].

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
