-module(ejabberd_auth_sso).

-behaviour(ejabberd_config).

-author('snorri.sturluson@ccpgames.com').

-behaviour(ejabberd_auth).

-export([start/1, stop/1, set_password/3, check_password/4,
    try_register/3, user_exists/2, remove_user/2,
    store_type/1, plain_password_required/1, opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Host) ->
    ?INFO_MSG("start: ~s", [Host]).

stop(Host) ->
    ?INFO_MSG("stop: ~s", [Host]).

plain_password_required(_) -> true.

store_type(_) -> external.


check_password(User, Server, Password) ->
    AllowAny = ejabberd_config:get_option({ssoauth_allow_any, Server}, false),
    case AllowAny of
        true -> ?INFO_MSG("Allow any, ignoring password", []), true;
        false -> check_password_sso(User, Server, Password)
    end.

check_password(User, AuthzId, Server, Password) ->
    if AuthzId /= <<>> andalso AuthzId /= User ->
        false;
        true -> check_password(User, Server, Password)
    end.


set_password(User, Server, _) ->
    ?INFO_MSG("set_password: ~s, ~s", [User, Server]),
    {error, db_failure}.

try_register(User, Server, _) ->
    ?INFO_MSG("try_register: ~s, ~s", [User, Server]),
    {error, db_failure}.

user_exists(User, Server) ->
    ?INFO_MSG("user_exists: ~s, ~s", [User, Server]),
    ok.

remove_user(User, Server) ->
    ?INFO_MSG("remove_user: ~s, ~s", [User, Server]),
    {error, db_failure}.


user_type("jumperbot_" ++ _) ->
    bot;

user_type("echobot") ->
    bot;

user_type("admin") ->
    admin;

user_type(_) ->
    user.


check_bot_password(Server, Password) ->
    case ejabberd_config:get_option({ssoauth_allow_bots, Server}, false) of
        true ->
            case string:equal(binary_to_list(Password), "ejabberd") of
                true -> true;
                false -> ?INFO_MSG("Password does not match", []), false
            end;
        false ->
            ?WARNING_MSG("Bots are not allowed", []),
            false
    end.


split_field(Field) ->
    SplitAt = string:str(Field, ":"),
    FieldName = string:strip(string:substr(Field, 1, SplitAt - 1), both, $"),
    Value = string:strip(string:substr(Field, SplitAt + 1), both, $"),
    {FieldName, Value}.


list_of_fields_to_dict(D, []) ->
    D;

list_of_fields_to_dict(D, [Field]) ->
    {Key, Value} = split_field(Field),
    dict:store(Key, Value, D);

list_of_fields_to_dict(D, [Field|Tail]) ->
    D2 = list_of_fields_to_dict(D, [Field]),
    list_of_fields_to_dict(D2, Tail).


parse_simple_json(Input) ->
    Stripped = string:strip(string:strip(Input, right, $}), left, ${),
    Fields = string:tokens(Stripped, ","),
    list_of_fields_to_dict(dict:new(), Fields).


fields_match([], _Desired, _Actual) ->
    true;

fields_match([Key], Desired, Actual) ->
    DesiredValue = dict:fetch(Key, Desired),
    ActualValue = dict:fetch(Key, Actual),
    DesiredValue == ActualValue;

fields_match([Key|Tail], Desired, Actual) ->
    case fields_match([Key], Desired, Actual) of
        true -> fields_match(Tail, Desired, Actual);
        false -> false
    end.


fields_match(Desired, Actual) ->
    Keys = dict:fetch_keys(Desired),
    fields_match(Keys, Desired, Actual).


validate_user(admin, Body) ->
    Actual = parse_simple_json(Body),
    Desired = dict:from_list([
        {"Scopes", "sso.generateAuthToken.v1"},
        {"TokenType", "Service"},
        {"ClientIdentifier", "eveServer"}
    ]),
    fields_match(Desired, Actual);

validate_user(User, Body) ->
    Actual = parse_simple_json(Body),
    Desired = dict:from_list([
        {"Scopes", ""},
        {"TokenType", "Character"},
        {"CharacterID", binary_to_list(User)}
    ]),
    case fields_match(Desired, Actual) of
        true ->
            %% Make sure UserID exists and is not empty
            case dict:find("UserID", Actual) of
                {ok, UserId} ->
                    true;
                _ ->
                    false
            end;
        false ->
            false
    end.


verify_token_on_endpoint(undefined, _User, _Token) ->
    ?ERROR_MSG("No endpoint defined for sso token verification", []),
    false;

verify_token_on_endpoint(VerifyEndpoint, User, Token) ->
    {Status, ResultOrReason} =
        httpc:request(get, {binary_to_list(VerifyEndpoint), [{"Authorization", binary_to_list(Token)}]}, [], []),
    case Status of
        ok ->
           {{_, ResponseCode, _}, _, Body}=ResultOrReason,
           case ResponseCode of
               200 ->
                   case validate_user(User, Body) of
                       true -> true;
                       false ->
                           ?INFO_MSG("Token is not valid: ~n~s", [Body]),
                           false
                   end;
               _ ->
                   ?INFO_MSG("Token is not valid: ~n~s", [Body]),
                   false
           end;
        _ -> ?ERROR_MSG("Error connecting to ~s to verify token~n~w", [VerifyEndpoint, Status]),
           false
    end.


check_sso_token(User, Server, Token) ->
    VerifyEndpoint = ejabberd_config:get_option({ssoauth_verify_endpoint, Server}),
    verify_token_on_endpoint(VerifyEndpoint, User, Token).


check_password_sso(User, Server, Password) ->
    case user_type(binary_to_list(User)) of
        bot -> check_bot_password(Server, Password);
        user -> check_sso_token(User, Server, Password);
        admin -> check_sso_token(admin, Server, Password)
    end.

-spec opt_type(ssoauth_allow_bots) -> fun((boolean()) -> boolean());
              (ssoauth_allow_any) -> fun((boolean()) -> boolean());
              (ssoauth_verify_endpoint) -> fun((binary()) -> binary());
	      (atom()) -> [atom()].
opt_type(ssoauth_allow_bots) -> fun(B) when is_boolean(B) -> B end;
opt_type(ssoauth_allow_any) -> fun(B) when is_boolean(B) -> B end;
opt_type(ssoauth_verify_endpoint) -> fun iolist_to_binary/1;
opt_type(_) -> [ssoauth_allow_bots, ssoauth_allow_any, ssoauth_verify_endpoint].

