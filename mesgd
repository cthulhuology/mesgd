#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa $(dirname 0)/ebin -noshell -noinput
%%
%% Copyright 2017 David J. Goehrig <dave@dloh.org>

%% starts a local node to issue commands from
%% we name these sequentially with system time
%% to avoid colliding with other command processes
connect(Server) ->
	net_kernel:start([ list_to_atom("cmd" ++ integer_to_list(erlang:system_time()) ++ "@localhost"), shortnames ]),
	set_cookie(),
	case net_kernel:connect(Server) of
		false -> 
			io:format("Failed to connect to ~p~n", [ Server ]),
			erlang:halt(0);
		true -> ok
	end.

process(Server,["node"]) ->
	process_flag(trap_exit,true),
	{ ok, _Pid } = net_kernel:start([ Server, shortnames ]),
	set_cookie(),
	receive
		'EXIT' -> 
			io:format("done~n"),
			erlang:halt(0)
	end;


%% process takes the command line arugments and performs
%% the associated action

%% init
process(_Server,["init","help"]) ->
	io:format("
orc [node] init [ nodes .. ]

	This command initializes an Mnesia database on the given node.
	The database is used to store users credentials and permissions.

");

process(Server,["init"|Cluster]) ->
	net_kernel:start([ list_to_atom("cmd" ++ integer_to_list(erlang:system_time()) ++ "@localhost"), shortnames ]),
	set_cookie(),
	Nodes = [ Server | lists:delete(Server,[ list_to_atom(X) || X <- Cluster ])],
	process_flag(trap_exit,true),
	io:format("Connecting to ~p~n", [ Nodes ]),
	[ net_kernel:connect(Node) || Node <- Nodes ],
	rpc:multicall(Nodes, application,load,[orc]),
	rpc:multicall([Server], orc_database,initialize, [Nodes]),
	green(ok),
	eol();

%% setup
process(_Server, [ "setup","help" ]) ->
	io:format("
orc setup file

	This command will load a file containing a list
	of users and their permissions. The format of
	this file is:

	{ users, [ { name, email, pass, paths... }, ... ]}.

");

process(Server, [ "setup", File ]) ->
	ok = connect(Server),
	{ ok, Config } = file:consult(File),
	io:format("loading config ~p~n", [ Config ]),
	Users = proplists:get_value(users, Config),
	[ setup_user(Server, User, Email, Pass, Paths) ||
		{ User, Email, Pass, Paths } <- Users ],
	rpc:call(Server,mnesia,sync_log,[]),
	io:format("done~n");


process(Server, [ "join" | Cluster ]) ->
	Nodes = [ list_to_atom(X) || X <- Cluster ],
	ok = connect(Server),
	rpc:multicall(Nodes,orc_cluster,add,[ Server ]),
	green(ok),
	eol();

process(Server, [ "leave" | Cluster ]) ->
	Nodes = [ list_to_atom(X) || X <- Cluster ],
	ok = connect(Server),
	rpc:multicall(Nodes,orc_cluster,remove,[ Server ]),
	green(ok),
	eol();

%% status
process(_Server,["status","help"]) ->
	io:format("
orc [node] status
	
	The status command will print ok or fail while attempting
	to connect to the given node or each node in the cluster.
	This does not indicate that the node is actually down, 
	just that it was not able to connect to the node.

");

process(Server,["status"]) ->
	io:format("~p: ", [ Server ] ),
	case connect(Server) of
		ok -> green(ok);
		_ -> red(fail)
	end,
	eol();

%% modules
process(Server,["modules", "list"] ) ->
	ok = connect(Server),
	List = rpc:call(Server,orc_dynamic,list,[]),
	io:format("~p modules: ~p~n", [ Server, List ]);

process(Server,["modules", "reload"]) ->
	ok = connect(Server),
	rpc:cast(Server,orc_dynamic,reload,[]),
	green(ok);	

%% observer
process(_Server,["observer","help"]) ->
	io:format("
orc [node] observer

	This command starts the observer on the given node.
	The Erlang observer is a standard gui for monitoring
	Erlang systems.

");

process(Server,["observer"]) ->
	ok = connect(Server),
	rpc:call(Server,observer,start,[]);

%% console
process(_Server,["console","help"]) ->
	io:format("
orc [node] console

	The console command connects to an erlang shell on the given node.
	It will take over the tty of the session and allows for total
	destruction of a node if you're into that sort of thing.

");

process(Server,["console"]) ->
	ok = connect(Server),
	process_flag(trap_exit,true),
	Shell = user_drv:start(['tty_sl -c -e',{Server,shell,start,[]}]),
	true = erlang:link(Shell),
	receive
		{ 'EXIT', Shell, _ } -> ok
	end,
	erlang:halt(0);

%% start
process(_Server,["start","help"]) ->
	io:format("
orc [node] start [port [cluster...]]
	
	The start command will start a orc application on the given node.
	Epmd must be running on the host already for this command to work.
	Instructions for setting up epmd on boot can be found in the README.

");	

process(Server,["start"]) ->
	process(Server,[ "start", "4433" ]);

process(Server,["start",Port|Cluster]) ->
	Nodes = [ Server | lists:delete(Server,[ list_to_atom(X) || X <- Cluster ])],
	ok = connect(Server),
	rpc:call(Server,application,set_env,[orc,port,list_to_integer(Port)]),
	rpc:call(Server,application,set_env,[orc,cluster, Nodes]),
	rpc:call(Server,application,set_env,[orc,path, "./priv" ]),
	rpc:call(Server,application,load,[orc]),
	rpc:call(Server,orc,start,[]);

%% stop
process(_Server,["stop","help"]) ->
	io:format("
orc [node] stop

	The stop command will stop a orc application on the given node.
	It will not stop epmd on the remote node, so you may call start 
	to start it again.

");

process(Server,["stop"]) ->
	case connect(Server) of
		ok ->
			rpc:call(Server, mnesia, stop, []),
			rpc:call(Server, orc, stop, []),
			rpc:call(Server, erlang, halt, [0]),
			green(ok),
			eol();
		fail ->
			red(fail),
			eol()
	end;
		
%% user
process(_Server,["user","help"]) ->
	io:format("
orc user [list|add|remove|grant|revoke]

	list				returns a list of all the usernames in the system
	add User Email Password		creates a new user with email and password
	auth Path User Password		test a user's auth credentials for a path
	remove User Email		removes the user with matching name and password
	grant User Path			grants a access to a path for a given user
	revoke User Path		removes access to a path for a given user

");

process(Server,["user","add",User,Email,Password]) ->
	ok = connect(Server),
	rpc:call(Server, orc_auth, add, [ User, Email, Password ]);

process(Server,["user","auth", Path, User, Password]) ->
	ok = connect(Server),
	Res = rpc:call(Server, orc_auth, test, [ Path, User, Password ]),
	io:format("~p~n", [ Res ]);

process(Server,["user","remove",User,Email]) ->
	ok = connect(Server),
	rpc:call(Server, orc_auth, remove,[ User, Email ]);

process(Server,["user","grant", User, Pattern ]) ->
	ok = connect(Server),
	rpc:call(Server, orc_auth, grant, [ User, Pattern ]);	

process(Server,["user","revoke", User, Pattern ]) ->
	ok = connect(Server),
	rpc:call(Server, orc_auth, revoke, [ User, Pattern ]);

process(Server,["user","list"]) ->
	ok = connect(Server),
	Users = rpc:call(Server, orc_auth, users, []),
	io:format("~p~n", [ Users ]);


%% This needs to be the last state in the process funciton, as it
%% is a catchall that prints out the status message.  If you are
%% adding new methods to it please add them above, and amend this
%% to reflect the new usage.
process(_Server,_) ->
	io:format("
usage: orc [node] [node|init|setup|start|stop|status|console|observer|user]

	help 				- this message

	command help			- help for the given command

	node 				- create a node

	init [cluster ...] 		- initialize orc database

	setup file			- load a setup file on a node

	start [ port [ cluster .. ]]	- start the server on a node

	stop 				- stop a node

	status 				- return the status of a node

	modules [ list | reload ]
		  list			- list all modules loaded
		  reload		- reload all dynamic modules
	
	join [ nodes ]			- joins a cluster 
	
	leave [ nodes ]			- leaves a cluster

	console 			- connects a console to a node

	observer 			- run the observer on a node

	user [node] [list|add|remove|grant|revoke]
		list			- list all users by name
		add user email pass	- add a user
		remove user email	- remove a user
		grant user path		- grant access to a path
		revoke user path	- remove access to a path
		auth path user pass	- test the user authentication

").

set_cookie() ->
	case application:get_env(orc,cookie) of
		{ ok, Cookie } ->
			erlang:set_cookie(node(),Cookie);
		_ ->
			ok
	end.

setup_user(Server,User,Email,Password,Paths) ->
	rpc:call(Server, orc_auth, add, [ User, Email, Password ]),
	[ rpc:call(Server, orc_auth, grant, [ User, Path ]) ||
		Path <- Paths ].

eol() ->
	io:format("~n").

green(Term) ->
	io:format([ 16#1b | "[;32m"]),
	io:format("~p", [ Term ]),
	io:format([ 16#1b | "[;39m" ]).

red(Term) ->
	io:format([ 16#1b | "[;31m"]),
	io:format("~p", [ Term ]),
	io:format([ 16#1b | "[;39m" ]).

find_server([]) -> 
	{ orc@localhost, []};
find_server(Args = [ Host | Args2 ]) ->
	case string:chr(Host,$@) of
		0 -> { orc@localhost, Args };
		_ -> { list_to_atom(Host), Args2 }
	end.

main(Args) ->
	%% always start a temporary node that we can discard
	Script = escript:script_name(),
	io:format("running ~p~n", [ Script] ),
	io:format("Directory ~p~n", [ filename:dirname(Script) ]),
	true = code:add_pathz(filename:dirname(escript:script_name()) ++ "/ebin"),
	{ Host, Args2 } = find_server(Args),
	process(Host,Args2).
