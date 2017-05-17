-module(mesgd_udp).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2017 David J. Goehrig"/utf8>>).
-behavior(gen_server).

-export([ start_link/3, send/3, stop/0, message/1 ]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

-include("../include/mesgd_http.hrl").

-record(mesgd_udp, { socket, path, protocol }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Methods
%%

%% Starts a wocket bsocket by accepting a connection form Listen port
start_link(Port,Path,Protocol) ->
	gen_server:start_link(?MODULE, [ Port, Path, Protocol ], []).

%% Sends data to the udp socket at host port
send(Host,Port,Data) ->
	gen_server:cast(?MODULE,{ send, Host, Port, Data }).

%% Stops the UDPSocket
stop() ->
	gen_server:cast(?MODULE,stop).

message(Data) ->
	gen_server:cast(?MODULE, { message,data }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server methods
init([ Port, Path, Protocol ]) ->
	{ ok, Socket } = gen_udp:open(Port),
	mesgd_router:connect(self(),Path),
	{ ok, #mesgd_udp{ socket = Socket, path = Path, protocol = Protocol } }.

handle_call(Message,_From,UDPSocket) ->
	error_logger:error_msg("Unknown message ~p", [ Message ]),
	{ reply, ok, UDPSocket }.
	
handle_cast( stop, UDPSocket ) ->
	{ stop, normal, UDPSocket };

handle_cast({ message, Data }, UDPSocket = #mesgd_udp{ socket = Socket, path = Path, protocol = Protocol }) ->	
	error_logger:info_msg("~p (~p) : ~p~n", [ Path, Protocol, Data ]),
	case Protocol of
		<<"ujson">> -> mesgd_router:route( ujson:decode(Data) );
		<<"json">> -> mesgd_router:route( json:decode(Data) );
		_ -> mesgd_router:route( [ { data, Data } ] )
	end,
	{ noreply, UDPSocket };

handle_cast({ send, Host, Port, Message }, UDPSocket = #mesgd_udp{ socket = Socket, protocol = Protocol }) ->
	Data = case Protocol of
		<<"ujson">> -> ujson:encode(Message);
		<<"json">> -> json:encode(Message);
		_ -> Message
	end,
	ok = gen_udp:send(Socket,Host,Port,Data),
	{ noreply, UDPSocket };

handle_cast({ unknown, Any }, UDPSocket) ->
	error_logger:error_msg("Unknown message ~p", [ Any ]),
	{ stop, unknown_message, UDPSocket };

handle_cast(close, UDPSocket) ->
	{ stop, normal, UDPSocket }.

handle_info({udp, Socket, Data }, UDPSocket) ->
	mesgd_router:route( [ { data, Data } ] ),
	{ noreply, UDPSocket };

handle_info(Message, UDPSocket) ->
	ok = gen_:send(UDPSocket,Message),
	{ noreply, UDPSocket }.

terminate( normal, UDPSocket = #mesgd_udp{ socket = Socket }) ->
	mesgd_router:close(self()),
	ssl:close(Socket),
	ok;

terminate( _Reason, #mesgd_udp{ socket = Socket }) ->
	mesgd_router:close(self()),
	ssl:close(Socket),
	ok.

code_change( _Old, UDPSocket, _Extra ) ->
	{ ok, UDPSocket }.

