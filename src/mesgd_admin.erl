-module(mesgd_admin).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2017 David J Goehrig"/utf8>>).
-behavior(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("include/mesgd_http.hrl").

-record(mesgd_api, { }).

start_link() ->
	gen_server:start_link(?MODULE,[],[]).

init([]) ->
	mesgd_router:mount(<<"admin">>,mesgd_user_api:init()),
	mesgd_router:mount(<<"admin">>,mesgd_auth_api:init()),
	mesgd_router:mount(<<"admin">>,mesgd_path_api:init()),
	mesgd_router:mount(<<"admin">>,mesgd_cluster_api:init()),
	{ ok, #mesgd_api{ } }.

handle_call(Message,_From,State) ->
	error_logger:error_msg("Unknown message ~p~n", [ Message ]),
	{ reply, ok, State }.

handle_cast(Message,State) ->
	error_logger:error_msg("Unknown message ~p~n", [ Message ]),
	{ noreply, State }.

handle_info(Message,State) ->
	error_logger:error_msg("Unknown message ~p~n", [ Message ]),
	{ noreply, State }.

terminate(Reason,_State) ->
	error_logger:info_msg("Stopping api ~p~n", [ Reason ]),
	ok.

code_change(_Old,_Extra,State) ->
	{ ok, State }.
