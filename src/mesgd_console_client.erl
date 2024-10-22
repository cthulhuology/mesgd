-module(mesgd_console_client).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2024 David J Goehrig"/utf8>>).
-export([ start/1,
	init/1, handle_call/3, handle_cast/2, handle_info/2,
	code_change/3, terminate/2
]).

-include("include/mesgd_http.hrl").
-record(mesgd_console_client, { socket, router, request = #request{}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

start(Socket) ->
	gen_server:start( ?MODULE, Socket, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%

init(Socket) ->
	ssl:controlling_process(Socket, self()),
	case ssl:handshake(Socket,1000) of
		{ ok , SSLSocket } ->
			{ ok, #mesgd_console_client{
				socket = SSLSocket,
				request = #request{ socket = Socket } }};
		{ error, timeout } ->
			error_logger:error_msg("SSl timeout~n"),
			{ stop, no_ssl };
		{ error, Reason } ->
			error_logger:error_msg("SSL connection failed: ~p", [ Reason ]),
			{ stop, Reason }
	end.

handle_call(Message,_From,Client) ->
	error_logger:error_msg("Unknown message: ~p", [ Message ]),
	{ reply, ok, Client }.

handle_cast(Message,Client) ->
	error_loggger:error_msg("Unknown message: ~p", [ Message ]),
	{ noreply, Client }.

handle_info({ ssl, Socket, Data }, Client = #mesgd_console_client{ request = Req }) ->
	Request = mesgd_http:request(Req,Data),
	case Request#request.stage of
		done ->
			%% need to grab the full request data
			mesgd_stats:record([{console_data_in, byte_size(Req#request.data)}, { console_http_in, 1}]),
			Response = mesgd_console_router:response(Request),
			error_logger:info_msg("Responding ~p with ~p", [ Request, Response ]),
			Bin = mesgd_http:response(Response),
			mesgd_stats:record([{ console_data_out, byte_size(Bin) }, { console_http_out, 1 }]),
			ssl:send(Socket,Bin),
			error_logger:info_msg("Sent data"),
			case ssl:shutdown(Socket, read_write) of
				ok -> 
					error_logger:info_msg("shutdown socket"),
					{ stop, normal, Client };
				{ error, Reason } ->
					error_logger:error_msg("Request for ~p failed: ~p", [ Request, Reason ]),
					ssl:close(Socket),
					{ stop, error, Client }
			end;
		_ ->
			%% partial request wait
			error_logger:info_msg("Waiting for more request data"),
			{ noreply, Client#mesgd_console_client{ request = Request }}
	end;

handle_info({ ssl_closed, _Socket}, Client = #mesgd_console_client{}) ->
	error_logger:info_msg("connection closed"),
	{ stop, normal, Client };

handle_info(Message,Client) ->
	error_logger:error_msg("Unknown message: ~p", [ Message ]),
	{ noreply, Client }.

terminate(_Reason,#mesgd_console_client{ socket = Socket }) ->
	error_logger:info_msg("terminating console client"),
	ssl:close(Socket),
	ok.

code_change( _Old, Client, _Extra) ->
	{ ok, Client }.
