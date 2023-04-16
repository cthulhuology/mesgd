-module(mesgd_router).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).

%% router behavior
-export([ start_link/0, stop/0, connect/2, route/1, close/1 ]).

%% gen_server behavior
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	terminate/2 ]).

-record(mesgd_router, { paths = []}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

start_link() ->
	gen_server:start_link({ local, ?MODULE }, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE,stop).

connect(Pid,Path) ->
	error_logger:info_msg("Connect Adding ~p for ~p~n", [ Path, Pid]),
	gen_server:cast(?MODULE,{ connect, Path, Pid }).

close(Pid) ->
	gen_server:cast(?MODULE, { close, Pid }).


route(<<>>) ->
	error_logger:info_msg("Attempt to route empty message~n"),
	ok;
route(Data) ->
	Nodes = mesgd_cluster:nodes(),
	error_logger:info_msg("Routing ~p to ~p~n", [Data,Nodes] ),
	gen_server:abcast(Nodes, ?MODULE, { route, Data }),
	gen_server:cast(?MODULE,{route,Data}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%
invoke({M,F},Data) ->
	apply(M,F,Data).

init([]) ->
	{ ok, #mesgd_router{ paths = [] }}.

handle_call(stop,_From,State) ->
	{ stop, stopped, State };

handle_call(Message,_From,State) ->
	error_logger:error_msg("Unknown message ~p", [ Message ]),
	{ reply, ok, State }.

handle_cast({ connect, Path, Pid }, State = #mesgd_router{ paths = Paths }) ->
	error_logger:info_msg("Adding ~p for ~p~n", [ Path, Pid ]),
	{ noreply, State#mesgd_router{ paths = [ { Pid, Path } | Paths ] } };	

handle_cast({ route, Data }, State = #mesgd_router{ paths = Paths }) ->
	error_logger:info_msg("Paths are ~p~n", [ Paths ]),
	[ mesgd_websocket:send(Pid, Data) || Pid <- mesgd_path:scan(Data,Paths), is_pid(Pid) ], 	
	[ invoke(Pid, Data) || Pid <- mesgd_path:scan(Data,Paths), is_tuple(Pid) ], 	
	{ noreply, State };

handle_cast({ close, Pid }, State = #mesgd_router{ paths = Paths } ) ->
	error_logger:info_msg("Removing ~p ~n", [ Pid ]),
	{ noreply, State#mesgd_router{ paths = proplists:delete(Pid,Paths) }};

handle_cast({ mount, Path, Module, Function }, State = #mesgd_router{ paths = Paths }) ->
	error_logger:info_msg("mounting ~p ~p:~p~n", [ Path, Module, Function ]),
	{ noreply, State#mesgd_router{ paths = [{ {Module,Function}, Path } | Paths ] }};

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
