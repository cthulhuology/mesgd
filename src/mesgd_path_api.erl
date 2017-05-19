-module(mesgd_path_api).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2017 David J Goehrig"/utf8>>).

-export([ init/0, get/1, put/1, post/1, delete/1 ]).

-include("../include/mesgd_http.hrl").

init() ->
	{ "/path/*", ?MODULE }.

get(Request = #request{ path = Path, headers = Headers, body = Body }) ->
	error_logger:info_msg("Got path request ~p~n", [ Request ]),
	<<"path api get~n">>.
	
put(Request = #request{ path = Path, headers = Headers, body = Body }) ->
	error_logger:info_msg("Got path request ~p~n", [ Request ]),
	<<"path api put~n">>.
	
post(Request = #request{ path = Path, headers = Headers, body = Body }) ->
	error_logger:info_msg("Got path request ~p~n", [ Request ]),
	<<"path api post~n">>.
	
delete(Request = #request{ path = Path, headers = Headers, body = Body }) ->
	error_logger:info_msg("Got path request ~p~n", [ Request ]),
	<<"path api delete~n">>.
	



