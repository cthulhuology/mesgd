-module(mesgd_console).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2024 David J Goehrig"/utf8>>).
-export([ start_link/0, stop/0,
	init/1, handle_call/3, handle_cast/2, handle_info/2,
	code_change/3, terminate/2
]).

-include("include/mesgd_http.hrl").
-record(mesgd_console, { socket }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

start_link() ->
	gen_server:start_link({ local, ?MODULE }, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, stop ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%

init([]) ->
	{ ok, Port } = mesgd_config:get(console_port),
	{ ok, CACert } = mesgd_config:get(console_cacert),
	{ ok, Cert } = mesgd_config:get(console_cert),
	{ ok, Key } = mesgd_config:get(console_key),
	case ssl:listen(Port, [
		binary,
		{packet,0},
		{certfile, Cert}, 
		{keyfile, Key},
		{cacertfile, CACert},
		{reuseaddr, true},
		{verify, verify_none}, 
		{fail_if_no_peer_cert, false}
	]) of
		{ ok, Socket } ->
			gen_server:cast(?MODULE,accept),
			{ ok, #mesgd_console{ socket = Socket }};
		{ error, Reason } ->
			error_logger:error_msg("Failed to start console on ~p because: ~p", [ Port, Reason]),
			{ stop, Reason }
	end.

handle_call(stop,_From,Server) ->
	error_logger:info_msg("Stopping"),
	{ stop, stopped, Server };

handle_call(Message,_From,Client) ->
	error_logger:error_msg("Unknown message: ~p", [ Message ]),
	{ reply, ok, Client }.

handle_cast(accept,Server = #mesgd_console{ socket = Socket }) ->
	mesgd_client:start_link(Socket),
	gen_server:cast(?MODULE,accept),
	{ noreply, Server };

handle_cast(Message,Server) ->
	error_logger:error_msg("Unknown message ~p", [ Message ]),
	{ noreply, Server }.

handle_info(Message,Server) ->
	error_logger:error_msg("Unknown message ~p", [ Message ]),
	{ noreply, Server }.

code_change(_Old,_Extra,Server) ->
	{ ok, Server }.

terminate(_Reason,_Server) ->
	ok.

