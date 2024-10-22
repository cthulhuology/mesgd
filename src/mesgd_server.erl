-module(mesgd_server).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start_link/0, stop/0 ]).
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	terminate/2 ]).

-record(mesgd_server, { port, socket, router, cacert, cert, key }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

start_link() ->
	{ ok, Port } = mesgd_config:get(port),
	{ ok, CACert } = mesgd_config:get(cacert),
	{ ok, Cert } = mesgd_config:get(cert),
	{ ok, Key } = mesgd_config:get(key),
	gen_server:start_link({ local, ?MODULE }, ?MODULE, #mesgd_server{
		port = Port,
		cacert = CACert,
		cert = Cert,
		key = Key
	}, []).

stop() ->
	gen_server:call(?MODULE,stop).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%

init(Server = #mesgd_server{ port = Port, cacert = CACert, cert = Cert, key = Key }) ->
	error_logger:info_msg("Starting mesgd server on port ~p~n", [ Port ]),
	case ssl:listen(Port,[
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
			gen_server:cast(?MODULE, listen),
			{ ok, Server#mesgd_server{ socket = Socket }};
		{ error, Reason } ->
			error_logger:error_msg("Socket listen failed on ~p because: ~p", [ Port, Reason ]),
			{ stop, Reason }
	end.

handle_call(stop,_From,Server) ->
	error_logger:info_msg("Stopping"),
	{ stop, stopped, Server };

handle_call(Message,_From,Server) ->
	error_logger:error_msg("Unknown message ~p", [ Message ]),
	{ reply, ok, Server }.

handle_cast(listen,Server = #mesgd_server{ socket = Listen }) ->
	case ssl:transport_accept(Listen) of
		{ ok, Socket } ->
			mesgd_client:start(Socket),
			gen_server:cast(?MODULE,listen),
			{ noreply, Server };
		{ error, Reason } ->
			{ stop, Reason, Server }
	end;

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
