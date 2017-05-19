-module(mesgd_client).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2017 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start_link/2  ]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

-include("include/mesgd_http.hrl").
-record(mesgd_client, { socket, domain, router, request = #request{} }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

start_link(Socket,Domain) ->
	gen_server:start_link(?MODULE, [Socket,Domain], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API

init([ Listen, Domain ]) ->
	case ssl:transport_accept(Listen) of
		{ ok, Socket } ->
			case ssl:ssl_accept(Socket,1000) of
				ok ->
					{ ok, #mesgd_client{
						socket = Socket,
						domain = Domain,
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
	
handle_call(Message,_From,Client) ->
	error_logger:error_msg("Unknown message: ~p", [ Message ]),
	{ reply, ok, Client }.

handle_cast(stop,State) ->
	error_logger:info_msg("Stopping"),
	{ stop, stopped, State };

handle_cast(Response = #response{},Client = #mesgd_client{ socket = Socket }) ->
	Bin = mesgd_http:response(Response),
	ssl:send(Socket,Bin),
	{ noreply, Client };

handle_cast(Message,Client) ->
	error_loggger:error_msg("Unknown message: ~p", [ Message ]),
	{ noreply, Client }.

handle_info({ ssl, Socket, Data }, Client = #mesgd_client{ request = Req }) ->
	Request = mesgd_http:request(Req,Data),
	case Request#request.stage of
		done ->
			handle_request(Request,Client),
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
	ssl:close(Socket),
	ok.

code_change( _Old, Client, _Extra) ->
	{ ok, Client }.


handle_request(Request = #request{ headers = Headers, socket = Socket }, #mesgd_client{ domain = Domain }) ->
	Request2 = mesgd_auth:auth(Domain,Request),
	Response = case mesgd_websocket:upgrade(Headers) of
		true ->	
			mesgd_websocket:response(Request2);
		_ ->
			mesgd_http_router:response(Domain,Request2)
	end,
	Bin = mesgd_http:response(Response),
	ssl:send(Socket,Bin).
