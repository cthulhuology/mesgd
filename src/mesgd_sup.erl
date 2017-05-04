-module(mesgd_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).


-define(ORC_SERVER(P), list_to_atom("mesgd_server_" ++ integer_to_list(P))).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ ok, Port} = application:get_env(mesgd,port),
	{ ok, Nodes } = application:get_env(mesgd,cluster),
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
		#{ id => mesgd_dynamic,
		start => { mesgd_dynamic, start_link, []},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [
			mesgd_dynamic,
			mesgd_static
		]},
		#{ id => mesgd_admin,
		start => { mesgd_admin, start_link, []},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [
			mesgd_admin
		]},
		#{ id => ?ORC_SERVER(Port),
		start => { mesgd_server, start_link, [ Port ]},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ 
			mesgd_server,
			mesgd
		]}
	]}}.
