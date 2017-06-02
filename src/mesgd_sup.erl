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
	{ ok, AdminPort } = application:get_env(mesgd,admin_port),
	{ ok, Nodes } = application:get_env(mesgd,cluster),
	{ ok, Master } = application:get_env(mesgd,master),
	{ok, { {one_for_one, 5, 10}, [
		#{ id => mesgd_cluster,
		start => { mesgd_cluster, start_link, [Nodes]},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [
			mesgd_cluster
		]},
		#{ id => mesgd_database,
		start => { mesgd_database, start_link, [ Nodes ]},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [
			mesgd_database
		]},
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
		#{ id => ?MESGD_HTTP_ROUTER("localhost"),
		start => { mesgd_http_router, start_link, [<<"localhost">>]},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [
			mesgd_http_router
		]},
		#{ id => ?MESGD_HTTP_ROUTER("admin"),
		start => { mesgd_http_router, start_link, [<<"admin">>]},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [
			mesgd_http_router
		]},
		#{ id => mesgd_admin,
		start => { mesgd_admin, start_link, []},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [
			mesgd_admin,
			mesgd_http_router
		]},
		#{ id => ?MESGD_SERVER(Port),
		start => { mesgd_server, start_link, [ 
			Port, <<"localhost">> ]},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ 
			mesgd_http_router,
			mesgd_server,
			mesgd
		]},
		#{ id => ?MESGD_SERVER(AdminPort),
		start => { mesgd_server, start_link, [
			AdminPort, <<"admin">> ]},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ 
			mesgd_admin_http_router,
			mesgd_server,
			mesgd
		]},
		#{ id => mesgd_startup,
		start => { mesgd_startup, start, [ Master ]},
		restart => transient,
		shutdown => brutal_kill,
		type => worker,
		modules => [
			mesgd_startup,
			mesgd_master
		]}
	]}}.
