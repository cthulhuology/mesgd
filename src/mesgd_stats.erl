-module(mesgd_stats).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2017 David J Goehrig"/utf8>>).
-behavior(gen_server).

-include("../include/mesgd_stats.hrl").

-export([start_link/1, install/1, record/1 ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(mesgd_stats_server, { period, last, 

%% start a stats server updating every Period seconds
start_link(Period) ->
	gen_server:start_link({ local, ?MODULE  }, ?MODULE, [ Period ], [] ).

record(Event) ->
	gen_server:cast(?MODULE, { event, Event }).

init([Period]) ->







install(Nodes) ->
	{ atomic, ok } = mnesia:create_table(mesgd_stats, [
		{ attributes, record_info(fields,mesgd_stats) },
		{ disc_copies, Nodes }]).
