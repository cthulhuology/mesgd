-module(mesgd_auth).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2017 David J Goehrig, Open Robotics Company LLC."/utf8>>).
-export([ install/1, auth/2, grant/3, revoke/3, token/2, tokenize/2, test/4, authorization/1 ]).

-include("include/mesgd_http.hrl").
-include("include/mesgd_auth.hrl").
-include("include/mesgd_user.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%
test(Path,Domain,User,Password) ->
	U = binary:list_to_bin(User),
	P = binary:list_to_bin(Password),
	Token = base64:encode(<< U/binary, ":", P/binary >>),
	case auth(Domain,#request{ path = Path, headers = [{ <<"Authorization">>, <<"Basic ", Token/binary >> }] }) of
		#request{} -> ok;
		_ -> fail
	end.

authorization(Headers) ->
	case proplists:get_value(<<"Authorization">>, Headers ) of
		undefined ->	%% if no auth was passed, we're going to add nothing to the domain
			<<"">>;
		Authorization ->
			[ <<"Basic">>,Auth ] = binary:split(Authorization,<<" ">>),
			Auth
	end.

tokenize(Domain,Auth) when is_list(Domain) ->
	tokenize(list_to_binary(Domain),Auth);
tokenize(Domain,Auth) when is_list(Auth) ->
	tokenize(Domain,list_to_binary(Auth));
tokenize(Domain,Auth) ->
	{ ok, Salt } = application:get_env(mesgd,salt),
	error_logger:info_msg("testing token ~p~n", [ Auth ]),
	Key = <<Domain/binary, Auth/binary>>,
	error_logger:info_msg("using key ~p~n", [ Key ]),
	crypto:hmac(sha256,Salt,Key).

auth(Domain,Request = #request{ path = Path, headers = Headers }) ->
	error_logger:info_msg("~p authenticating ~p~n", [ Domain, Request ]),	
	Auth = authorization(Headers),
	error_logger:info_msg("Found authorization token ~p~n", [ Auth ]),
	Token = tokenize(Domain,Auth),
	error_logger:info_msg("Token: ~p~n", [ Token ]),
	F = fun() ->
		case mnesia:read(mesgd_auth,Token) of
			[ #mesgd_auth{ user = User, email = Email, active = true, paths = Paths } ] ->
				case lists:foldl(fun(Pattern,Match) -> 
					mesgd_path:eval(Path,Pattern) or Match end, false, Paths) of
					true ->
						error_logger:info_msg("Allow ~p <~p> for ~p", [ User, Email, Path ]),
						mesgd_stats:record([{auth_ack,1}]),
						Request#request{ headers = [ { <<"User">>, User},{<<"Email">>,Email } | Headers ] };
					false ->
						error_logger:info_msg("Deny ~p <~p> for ~p", [ User, Email, Path ]),
						mesgd_stats:record([{auth_nak,1}]),
						#response{ status = 401, headers = [{  <<"WWW-Authenticate">>,<<"Basic realm=\"mesgd\"">> }, {<<"Content-Length">>, <<"0">> }]}
				end;
			Any ->
				error_logger:info_msg("Deny token ~p for ~p cause ~p", [ Auth, Path, Any ]),
				mesgd_stats:record([{auth_nak,1}]),
				#response{ status = 401, headers = [{  <<"WWW-Authenticate">>,<<"Basic realm=\"mesgd\"">> }, {<<"Content-Length">>, <<"0">> }]}
		end
	end,
	mnesia:activity(transaction,F).

grant(Domain,User,Pattern) when is_list(Domain)->
	grant(list_to_binary(Domain),User,Pattern);
grant(Domain,User,Pattern) when is_list(User) ->
	grant(Domain,list_to_binary(User),Pattern);
grant(Domain,User,Pattern) ->
	F = fun() ->
		case mnesia:match_object(#mesgd_auth{ domain = Domain, user = User, email = '_', token = '_', active = true, paths = '_' }) of
			[] -> ok;
			[ Auth = #mesgd_auth{ paths = Paths } ] ->
				ok = mnesia:write(Auth#mesgd_auth{ paths = [ Pattern | Paths ]}),
				error_logger:info_msg("Granted ~p access to ~p", [ User, Pattern ])	
		end
	end,
	mnesia:activity(transaction,F).

revoke(Domain,User,Pattern) when is_list(Domain) ->
	revoke(list_to_binary(Domain),User,Pattern);
revoke(Domain,User,Pattern) when is_list(User) ->
	revoke(Domain,list_to_binary(User), Pattern);
revoke(Domain,User,Pattern) ->
	F = fun() ->
		case mnesia:match_object(#mesgd_auth{ domain = Domain, user = User, email = '_', token = '_', active = true, paths = '_' }) of
			[] -> ok;
			[ Auth = #mesgd_auth{ paths = Paths }] ->
				ok = mnesia:write(Auth#mesgd_auth{ paths = lists:delete(Pattern,Paths) }),
				error_logger:info_msg("Revoked ~p access to ~p", [ User, Pattern ])
		end
	end,
	mnesia:activity(transaction,F).

token(Domain,User) when is_list(Domain) ->
	token(list_to_binary(Domain),User);
token(Domain,User) when is_list(User) ->
	token(Domain,list_to_binary(User));
token(Domain,User) ->
	F = fun() ->
		case mnesia:match_object(#mesgd_auth{ domain = Domain, user = User, email = '_', token = '_', active = true, paths = '_' }) of
			[ #mesgd_auth{ token = Token }] ->
				Token;
			_ ->
				undefined
		end
	end,
	mnesia:activity(transaction,F).

install(Nodes) ->
	{ atomic, ok } = mnesia:create_table(mesgd_auth, [
		{ attributes, record_info(fields,mesgd_auth) },
		{ disc_copies, Nodes }]).
