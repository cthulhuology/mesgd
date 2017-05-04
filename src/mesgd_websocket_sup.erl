-module(mesgd_websocket_sup).
-behaviour(supervisor).
-export([start_link/0, client/1 ]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE,[]).

init([]) ->
	{ ok, { {one_for_one, 5, 10}, [] } }.


client(Request) ->
	error_logger:info_msg("Supervisor starting child  ~p", [ Request ]),
	supervisor:start_child(?MODULE, #{
		id => uuid:id(),
		start => { mesgd_websocket, start_link, [ Request ]},
		restart => temporary,
		shutdown => brutal_kill,
		type => worker,
		modules => [
			mesgd_websocket
		]
	}).
