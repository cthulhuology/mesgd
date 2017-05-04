-module(mesgd_static).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2017 David J. Goehrig"/utf8>>).
-export([ get/1, static_file/1, default_file/1, file/1 ]).

-include("include/mesgd_http.hrl").

get(Request = #request {}) ->	
	static_file(Request).

default_file(#request{ path = Path, headers = Headers }) ->
	Host = proplists:get_value(<<"Host">>, Headers),
	PathBin = binary:list_to_bin(Path),
	<<"<script>ws = new WebSocket('wss://", Host/binary, PathBin/binary,
		"','json'); ws.onmessage = function(msg) {"
		" console.log(JSON.parse(msg.data)) };</script>">>.

file(Path) ->
	
	Filename = mesgd_path:priv() ++ "/html" ++ Path,
	error_logger:info_msg("Looking for file ~p~n",[ Filename ]),
	file:read_file(Filename).

static_file(Request = #request{ path = Path }) ->
	case file(Path) of
		{ ok, Bin } ->
			Bin;
		{ error, Error } ->
			error_logger:error_msg("Returning default ~p~n", [ Error ]),
			default_file(Request)
	end.
