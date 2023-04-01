-module(mesgd_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).


-define(MESGD_SERVER(P), list_to_atom("mesgd_server_" ++ integer_to_list(P))).
-define(MESGD_HTTP_ROUTER(D), list_to_atom("mesgd_http_router_" ++ D)).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ ok, Port} = application:get_env(mesgd,port),
	{ ok, CACert } = application:get_env(mesgd,cacertfile),
	{ ok, Cert } = application:get_env(mesgd,certfile),
	{ ok, Key } = application:get_env(mesgd,keyfile),
	{ ok,PublicKeyFile } = application:get_env(mesgd,publickey),
	{ ok, PrivateKeyFile } = application:get_env(mesgd,privatekey),
	{ok, { {one_for_one, 5, 10}, [
		#{ id => mesgd_websocket_sup,
		start => { mesgd_websocket_sup, start_link, []},
		restart => permanent,
		shutdown => brutal_kill,
		type => supervisor,
		modules => [
			mesgd_websocket_sup
		]},
		#{ id => mesgd_router,
		start => { mesgd_router, start_link, []},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ 
			mesgd_router
		]},
		#{ id => ?MESGD_SERVER(Port),
		start => { mesgd_server, start_link, [ 
			Port, <<"localhost">>, CACert, Cert, Key ]},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ 
			mesgd_http_router,
			mesgd_server,
			mesgd
		]},
		#{ id => mesgd_jwt,
		start => { mesgd_start, start_link, [ PublicKeyFile, PrivateKeyFile ]},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ jwt ]}
	]}}.
