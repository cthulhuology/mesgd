-module(mesgd_cluster_api).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2017 David J Goehrig"/utf8>>).

-export([ init/0, get/1, put/1, post/1, delete/1 ]).

-include("../include/mesgd_http.hrl").

init() ->
	{ "/cluster/*", ?MODULE }.

get(Request = #request{ path = Path, headers = Headers, body = Body }) ->
	error_logger:info_msg("Got cluster request ~p~n", [ Request ]),
	<<"cluster api get~n">>.
	
put(Request = #request{ path = Path, headers = Headers, body = Body }) ->
	error_logger:info_msg("Got cluster request ~p~n", [ Request ]),
	<<"cluster api put~n">>.
	
post(Request = #request{ path = Path, headers = Headers, body = Body }) ->
	error_logger:info_msg("Got cluster request ~p~n", [ Request ]),
	<<"cluster api post~n">>.
	
delete(Request = #request{ path = Path, headers = Headers, body = Body }) ->
	error_logger:info_msg("Got cluster request ~p~n", [ Request ]),
	<<"cluster api delete~n">>.
	



