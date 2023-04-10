-module(mesgd_stats).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2012,2013,2023 David J. Goehrig"/utf8>>).
-behavior(gen_server).

-export([ record/1, start_link/0, stop/0 ]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

%%
%% the idea behind stats is if no one is listening they all go to /dev/null
%% otherwise if you're paying attention you get flooding with messages on
%% the stats channel, you can subscribe to the staus you want with path args
%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Methods
%%

%% Starts a websocket by accepting a connection form Listen port
start_link() ->
	gen_server:start_link(?MODULE, [], []).

%% Stop the logging
stop() ->
	gen_server:cast(?MODULE,stop).


record(List) ->
	gen_server:cast(?MODULE,{ record, List }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Methods

init([]) ->
	{ ok, [] }.

handle_call(Msg,_,State) ->
	error_logger:error_msg("Unknown call ~p~n", [ Msg ]),
	{ reply, ok, State }.

handle_cast({ record, List }, State ) ->
	[ mesgd_router:route(statistic(H)) || H <- List ],
	{ noreply, State };

handle_cast(Msg, State) ->
	error_logger:error_msg("Unknown cast ~p~n", [ Msg ]),
	{ noreply, State }.	

handle_info(Msg, State) ->
	error_logger:error_msg("Unknown info ~p~n", [ Msg ]),
	{ noreply, State }.	

terminate( _, _ )->
	ok.

code_change( _Old, WebSocket, _Extra ) ->
	{ ok, WebSocket }.

statistic({ Category, Value }) ->
	json:encode([{ <<"mesgd/statistics">>, atom_to_binary(Category) }, {atom_to_binary(Category), Value }]).
