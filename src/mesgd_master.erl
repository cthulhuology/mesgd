-module(mesgd_master).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2017 David J Goehrig, Open Robotics Company LLC."/utf8>>).
-behavior(gen_server).
-export([ start_link/0, add/3, remove/3, install/1, log/1 ]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

-record(mesgd_master, { nodes }).
-record(mesgd_node, { name, domain, ip }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API

start_link() ->
	gen_server:start_link({local,?MODULE},?MODULE,[],[]).

add(Name,Domain,Ip) ->
	gen_server:call(?MODULE, { add, Name, Domain, Ip }).

remove(Name,Domain,Ip) ->
	gen_server:call(?MODULE, { remove, Name, Domain, Ip }).

log(Message) ->
	gen_server:cast(?MODULE, { log, Message }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%

init([]) ->
	{ ok, #mesgd_master{} }.

handle_call({ add, Name, Domain, Ip }, _From, Master = #mesgd_master{ nodes = Nodes }) ->
	Node = #mesgd_node{ name = Name, domain = Domain, ip = Ip },
	F = fun() ->
		ok = mnesia:write(Node)
	end,
	mnesia:activity(transaction,F),
	{ reply, ok, Master#mesgd_master{ nodes = [ Node | Nodes ] } };

handle_call({ remove, Name, Domain, Ip }, _From, Master = #mesgd_master{ nodes = Nodes }) ->
	Node = #mesgd_node{ name = Name, domain = Domain, ip = Ip },
	F = fun() ->
		ok = mnesia:delete(mesgd_master, Name, write)
	end,
	mnesia:activity(transaction,F),
	{ reply, ok, Master#mesgd_master{ nodes = lists:delete(Node,Nodes) } };

handle_call(Message,_From,Master) ->
	error_logger:error_msg("Unknown message ~p~n", [ Message ]),
	{ reply, ok, Master }.

handle_cast({log, Message},Master) ->
	error_logger:info_msg("Stats: ~p~n", [ Message ]),
	{ noreply, Master };

handle_cast(Message,Master) ->
	error_logger:error("Unknown message ~p~n", [ Message ]),
	{ noreply, Master }.

handle_info(Message,Master) ->
	error_logger:error("Unknown message ~p~n", [ Message ]),
	{ noreply, Master }.

terminate(_Reason,_Master) ->
	ok.

code_change(_Old,_Extra,Master) ->
	{ ok, Master }.


install(Nodes) ->
	{ ok, Master } = application:get_env(mesgd,is_master),
	case Master of
		true -> 
			error_logger:info_msg("Creating a master database"),
			{ atomic, ok } = mnesia:create_table(mesgd_node, [
				{ attributes, record_info(fields,mesgd_node) },
				{ disc_copies, Nodes }]);
		_ ->
			error_logger:info_msg("Skipping master database")
	end.
			
