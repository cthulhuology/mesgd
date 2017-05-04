-module(mesgd).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start/0, start_link/1, stop/0, server/1 ]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

-include("include/mesgd_http.hrl").
-record(mesgd, { socket, request = #request{} }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

%% create a mesgd_server under supervision for the port
server(Port) ->
	mesgd_sup:server(Port).

start() ->
	application:ensure_all_started(mesgd).

start_link(Socket) ->
	gen_server:start_link(?MODULE, #mesgd{ 
		socket = Socket,
		request = #request{}
	},[]).

stop() ->
	application:stop(mesgd).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%

init(Orc = #mesgd{ socket = Listen }) ->
	case ssl:transport_accept(Listen) of
		{ ok, Socket } ->
			case ssl:ssl_accept(Socket,1000) of
				ok ->
					{ ok, Orc#mesgd{
						socket = Socket,
						request = #request{ socket = Socket }
					}};
				{ error, timeout } ->
					error_logger:error_msg("SSl timeout~n"),
					{ stop, no_ssl };
				{ error, Reason } ->
					error_logger:error_msg("SSL connection failed: ~p", [ Reason ]),
					{ stop, Reason }
			end;
		{ error, Reason } ->
			error_logger:error_msg("Socket accept failed: ~p", [ Reason ]),
			{ stop, Reason }
	end.
	
handle_call(Message,_From,Orc) ->
	error_logger:error_msg("Unknown message: ~p", [ Message ]),
	{ reply, ok, Orc }.

handle_cast(stop,State) ->
	error_logger:info_msg("Stopping"),
	{ stop, stopped, State };

handle_cast(Response = #response{},Orc = #mesgd{ socket = Socket }) ->
	Bin = mesgd_http:response(Response),
	ssl:send(Socket,Bin),
	{ noreply, Orc };

handle_cast(Message,Orc) ->
	error_loggger:error_msg("Unknown message: ~p", [ Message ]),
	{ noreply, Orc }.

handle_info({ ssl, Socket, Data }, Orc = #mesgd{ request = Req }) ->
	Request = mesgd_http:request(Req,Data),
	case Request#request.stage of
		done ->
			handle_request(Request),
			{ noreply, Orc#mesgd{ request = #request{ socket = Socket } }};
		_ ->
			{ noreply, Orc#mesgd{ request = Request }}
	end;

handle_info({ ssl_closed, _Socket}, Orc = #mesgd{}) ->
	error_logger:info_msg("connection closed"),
	{ stop, normal, Orc };

handle_info(Message,Orc) ->
	error_logger:error_msg("Unknown message: ~p", [ Message ]),
	{ noreply, Orc }.

terminate(_Reason,#mesgd{ socket = Socket }) ->
	ssl:close(Socket),
	ok.

code_change( _Old, Orc, _Extra) ->
	{ ok, Orc }.


handle_request(Request = #request{headers = Headers, socket = Socket }) ->
	case proplists:get_value(<<"Upgrade">>, Headers) of
		<<"websocket">> ->
			Response = mesgd_websocket:get(Request),
			Bin = mesgd_http:response(Response),
			ssl:send(Socket,Bin);
		_ ->
			Response = mesgd_dynamic:get(Request),
			Bin = mesgd_http:response(Response),
			ssl:send(Socket,Bin),
			ssl:close(Socket)
	end.

