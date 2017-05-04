-module(mesgd_server).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start_link/1, stop/1, accept/1 ]).
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	terminate/2 ]).

-define(SELF, list_to_atom(?MODULE_STRING ++ "_" ++ integer_to_list(Port))).
-record(mesgd_server, { port, socket }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

start_link(Port) ->
	gen_server:start_link({ local, ?SELF }, ?MODULE, #mesgd_server{
		port = Port
	}, []).

stop(Port) ->
	gen_server:call(?SELF,stop).

accept(Port) ->
	gen_server:cast(?SELF,accept).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%

init(Server = #mesgd_server{ port = Port }) ->
	Directory = mesgd_path:priv(),
	CACert = Directory ++ "/cacert.pem",
	Cert = Directory ++ "/cert.pem",
	Key = Directory ++ "/key.pem",
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
	%	{versions,['tlsv1.2']},
	%	{ciphers,[{rsa,aes_128_cbc,sha}]}
	]) of
		{ ok, Socket } ->
			accept(Port),
			{ ok, Server#mesgd_server{ socket = Socket }};
		{ error, Reason } ->
			error_logger:error_msg("Socket listen failed on ~p because: ~p", [ Port, Reason ]),
			{ stop, Reason }
	end.

handle_call(stop,_From,Server) ->
	{ stop, stopped, Server };

handle_call(Message,_From,Server) ->
	error_logger:error_msg("Unknown message ~p", [ Message ]),
	{ reply, ok, Server }.

handle_cast(accept,Server = #mesgd_server{ socket = Socket, port = Port }) ->
	mesgd:start_link(Socket),
	accept(Port),
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
