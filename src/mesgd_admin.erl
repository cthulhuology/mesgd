-module(mesgd_admin).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2024 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start_link/0, admin/1, stop/0 ]).
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	terminate/2 ]).

-record(mesgd_admin, { admins = [ "admin@localhost" ] }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

%% Start the admin server
start_link() ->
	gen_server:start_link({ local, ?MODULE }, ?MODULE, [], []).

%% Handle admin messages
admin(Data) ->
	gen_server:cast(?MODULE, { admin, Data }).	

%% stop the admin server
stop() ->
	gen_server:call(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%

init([]) ->
	%% listen to all of the mesgd admin messages
	mesgd_pki:load("admin@localhost","admin.pem"),
	mesgd_router:connect({ ?MODULE, admin },"/mesgd/admin"),
	{ ok, #mesgd_admin{} }.

handle_call(stop,_From,State) ->
	{ stop, stopped, State };

handle_call(Message,_From,State) ->
	error_logger:error_msg("Unknown message ~p", [ Message ]),
	{ reply, ok, State }.

handle_cast({admin, Data }, State = #mesgd_admin{ }) ->
	error_log:info_msg("ADMIN: ~p~n",[ Data ]),
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

