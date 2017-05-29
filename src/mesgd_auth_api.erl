-module(mesgd_auth_api).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2017 David J Goehrig"/utf8>>).

-export([ init/0, get/1, put/1, post/1, delete/1 ]).

-include("../include/mesgd_http.hrl").

init() ->
	{ "/auth/*", ?MODULE }.

get(Request = #request{ path = _Path, headers = _Headers, body = _Body }) ->
	error_logger:info_msg("Got auth request ~p~n", [ Request ]),
	<<"auth api get~n">>.
	
put(Request = #request{ path = _Path, headers = _Headers, body = _Body }) ->
	error_logger:info_msg("Got auth request ~p~n", [ Request ]),
	<<"auth api put~n">>.
	
post(Request = #request{ path = _Path, headers = _Headers, body = _Body }) ->
	error_logger:info_msg("Got auth request ~p~n", [ Request ]),
	<<"auth api post~n">>.
	
delete(Request = #request{ path = _Path, headers = _Headers, body = _Body }) ->
	error_logger:info_msg("Got auth request ~p~n", [ Request ]),
	<<"auth api delete~n">>.
	



