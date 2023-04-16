-module(mesgd_user_api).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2017 David J Goehrig"/utf8>>).

-export([ init/0, get/1, put/1, post/1, delete/1 ]).

-include("../include/mesgd_http.hrl").

init() ->
	{ "/user/*", ?MODULE }.

get(#request{ path = Path, headers = _Headers, body = _Body }) ->
	case string:tokens(Path,"/") of
		[ "user" ] ->
			error_logger:info_msg("Looking up all users"),
			json:encode(mesgd_user:users());
		[ "user", Name ] ->
			User = mesgd_user:user(Name),
			json:encode(User);
		_ -> 	
			json:encode([])
	end.
	
put(#request{ path = Path, headers = _Headers, body = Body }) ->
	case string:tokens(Path,"/") of
		[ "user", Name ] ->
			UserName = list_to_binary(Name),
			User = json:decode(Body),
			UserName = proplists:get_value(<<"name">>,User),
			Email = proplists:get_value(<<"email">>,User),
			Password = proplists:get_value(<<"password">>,User),
			Paths = proplists:get_value(<<"paths">>, User),
			ok = mesgd_user:add(Name,Email,Password),
			[ mesgd_auth:grant(Name,P) || P <- Paths ],
			json:encode(mesgd_user:user(Name));
		_ -> 
			json:encode([])
	end.
	
post(Request = #request{ path = _Path, headers = _Headers, body = _Body }) ->
	error_logger:info_msg("Got user request ~p~n", [ Request ]),
	<<"user api post~n">>.
	
delete(Request = #request{ path = _Path, headers = _Headers, body = _Body }) ->
	error_logger:info_msg("Got user request ~p~n", [ Request ]),
	<<"user api delete~n">>.
	



