-module(mesgd_client).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2017 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start/1  ]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

-include("include/mesgd_http.hrl").
-record(mesgd_client, { socket, router, request = #request{} }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

start(Socket) ->
	gen_server:start(?MODULE, Socket, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%

init(Socket) ->
	ssl:controlling_process(Socket, self()),
	case ssl:handshake(Socket,1000) of
		{ ok , SSLSocket } ->
			{ ok, #mesgd_client{
				socket = SSLSocket,
				request = #request{ socket = Socket }
			}};
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

handle_info({ ssl, Socket, Data }, Client = #mesgd_client{ request = Req }) ->
	Request = mesgd_http:request(Req,Data),
	case Request#request.stage of
		done ->
			%% need to grab the full request data
			mesgd_stats:record([{data_in, byte_size(Req#request.data)}, { http_in, 1}]),
			Response = case mesgd_auth:auth(Request) of
				invalid -> 
					#response{ status =  401 };
				Claims ->
					case mesgd_websocket:upgrade(Request#request.headers) of
						true ->	
							mesgd_websocket:response(Request#request{ claims = Claims });
						_ ->
							mesgd_http_router:response(Request#request{ claims = Claims })

					end
			end,
			Bin = mesgd_http:response(Response),
			mesgd_stats:record([{ data_out, byte_size(Bin) }, { http_out, 1 }]),
			ssl:send(Socket,Bin),
			{ noreply, Client#mesgd_client{ request = #request{ socket = Socket } }};
		_ ->
			{ noreply, Client#mesgd_client{ request = Request }}
	end;

handle_info({ ssl_closed, _Socket}, Client = #mesgd_client{}) ->
	error_logger:info_msg("connection closed"),
	{ stop, normal, Client };

handle_info(Message,Client) ->
	error_logger:error_msg("Unknown message: ~p", [ Message ]),
	{ noreply, Client }.

terminate(_Reason,#mesgd_client{ socket = Socket }) ->
	ssl:shutdown(Socket,read_write).

code_change( _Old, Client, _Extra) ->
	{ ok, Client }.
