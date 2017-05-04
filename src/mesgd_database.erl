-module(mesgd_database).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ initialize/1, start_link/1, stop/0 ]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

-record(mesgd_database, { nodes, tables }).


start_link(Nodes) ->
	gen_server:start_link({ local, ?MODULE }, ?MODULE, Nodes, []).

stop() ->
	gen_server:call(?MODULE,stop).

%% Initializes a blank db on all of the configured nodes
initialize(Nodes) ->
	io:format("Setting up nodes ~p~n", [ Nodes ]),
	{ ok, Tables } =  application:get_env(mesgd,tables),	% the tables contains a list of all modules with install functions
	rpc:multicall(Nodes, mnesia,stop, []),
	mnesia:delete_schema(Nodes),
	mnesia:create_schema(Nodes),
	rpc:multicall(Nodes, mnesia ,start, []),
	[ Table:install(Nodes) || Table <- Tables ],
	rpc:multicall(Nodes, mnesia,stop, []),
	ok.

%% ensures all database tables are running before we use it
init(Nodes) ->
	error_logger:info_msg("Starting database on ~p~n", [ Nodes ]),
	{ ok, Tables } =  application:get_env(mesgd,tables),
	{ ok, Timeout } = application:get_env(mesgd,table_timeout),
	rpc:multicall(Nodes,mnesia,start,[]),
	case mnesia:wait_for_tables(Tables,Timeout) of
		ok ->
			error_logger:info_msg("Database started on ~p", [ Nodes ]),
			{ ok, #mesgd_database{ nodes = Nodes, tables = Tables }};
		{ timeout, Remaining } ->
			error_logger:error_msg("Database failed to load ~p", [ Remaining ]),		
			{ stop, timeout }
	end.

handle_call(stop,_From,State)  ->
	{ stop, stopped, State };
handle_call(Message,_From,State) ->
	error_logger:error_msg("Unknown message: ~p", [ Message ]),
	{ reply, ok, State }.
	
handle_cast(Message,State) ->
	error_logger:error_msg("Unknown message: ~p", [ Message ]),
	{ noreply, State }.
	
handle_info(Message,State) ->
	error_logger:error_msg("Unknown message: ~p", [ Message ]),
	{ noreply, State }.

code_change(_Old,_Extra,State) ->
	{ ok, State }.


terminate(Reason,#mesgd_database{ nodes = Nodes } = State) ->
	error_logger:info_msg("Terminating ~p with ~p", [ Reason, State ]),
	rpc:multicall(Nodes,application,stop, [ mnesia ]),
	ok.

