
-module(mesgd_pki_server).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2024 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start_link/0, pki/1, stop/0 ]).
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	terminate/2 ]).

-record(mesgd_pki, { private = [], public = []  }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

%% Start the key server
start_link() ->
	gen_server:start_link({ local, ?MODULE }, ?MODULE, [], []).

%% Handle a pki message
pki(Data) ->
	gen_server:cast(?MODULE, { pki, Data }).

%% Stop the key server
stop() ->
	gen_server:cast(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%

init([]) ->
	%% listen to all of the mesgd pki messages
	mesgd_router:connect({ ?MODULE, pki },"/mesgd/pki"),
	{ ok, #mesgd_pki{} }.

handle_call(stop,_From,State) ->
	{ stop, stopped, State };

handle_call(Message,_From,State) ->
	error_logger:error_msg("Unknown message ~p", [ Message ]),
	{ reply, ok, State }.

handle_cast({pki, Data }, State = #mesgd_pki{}) ->
	error_log:info_msg("PKI: ~p~n",[ Data ]),
	{ noreply, State };

handle_cast(Message,State) ->
	error_logger:error_msg("Unknown message ~p", [ Message ]),
	{ noreply, State }.

handle_info(Message,State) ->
	error_logger:error_msg("Unknown message ~p", [ Message ]),
	{ noreply, State }.

code_change(_Old,_Extra,State) ->
	{ ok, State }.

terminate(_Reason,_State) ->
	ok.
