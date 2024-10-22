-module(mesgd_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% TODO update the server init to make use of the config file
init([]) ->
	{ok, Nodes} = mesgd_config:get(nodes),
	{ok, { {one_for_one, 5, 10}, [
		#{ id => mesgd_websocket_sup,
		start => { mesgd_websocket_sup, start_link, []},
		restart => permanent,
		shutdown => brutal_kill,
		type => supervisor,
		modules => [
			mesgd_websocket_sup
		]},
		#{ id => mesgd_stats,
		start => { mesgd_stats, start_link, []},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [
			mesgd_stats
		]},
		#{ id => mesgd_auth,
		start => { mesgd_auth, start_link, []},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [
			mesgd_config
		]},
		#{ id => mesgd_router,
		start => { mesgd_router, start_link, []},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ 
			mesgd_router
		]},
		#{ id => mesgd_server,
		start => { mesgd_server, start_link, []}, 
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ 
			mesgd_http_router,
			mesgd_server,
			mesgd
		]},
		#{ id => mesgd_http_router,
		start => { mesgd_http_router, start_link, []}, 
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ 
			mesgd_path
		]},
		#{ id => mesgd_jwt,
		start => { mesgd_jwt, start_link, []},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ jwt ]},
		#{ id => mesgd_pki,
		start => { mesgd_pki, start_link, []},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ jwk ]},
		#{ id => mesgd_pki_server,
		start => { mesgd_pki_server, start_link, []},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ mesgd_pki ]},
		#{ id => mesgd_admin,
		start => { mesgd_admin, start_link, []},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ mesgd_pki ]},
		#{ id => mesgd_console_router,
		start => { mesgd_console_router, start_link, []},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ ]},
		#{ id => mesgd_console,
		start => { mesgd_console, start_link, []},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ 
			mesgd_console_client,
			mesgd_console_router
		]},
		#{ id => mesgd_cluster,
		start => { mesgd_cluster, start_link, [ Nodes ]},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [  ]}
	]}}.
