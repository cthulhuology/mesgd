-module(mesgd_startup).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2017 David J Goehrig, Open Robotics Company LLC."/utf8>>).
-export([ start/1, install/1 ]).

-include("include/mesgd_http.hrl").
-include("include/mesgd_auth.hrl").
-include("include/mesgd_user.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%
start(Master) ->
	case Master of
		undefined ->
			error_logger:info_msg("Starting as master"),
			start_master();
		_ ->
			error_logger:info_msg("Connecting to ~p~n", [ Master]),
			connect_master(Master)
	end.


install(Nodes) ->
	{ atomic, ok } = mnesia:create_table(mesgd_auth, [
		{ attributes, record_info(fields,mesgd_auth) },
		{ disc_copies, Nodes }]).
			
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%

start_master() ->
	case gen_supervisor:start_child(mesgd_sup,#{ 
		id => mesgd_master,
		start => { mesgd_master, start_link, []},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [
			mesgd_master
		]}) of
		{ ok, Child } ->
			error_logger:info_msg("Started mesgd_master ~p~n", [ Child ]);
		{ ok, Child, _Info } ->
			error_logger:info_msg("Started mesgd_master ~p~n", [ Child ]);
		{ error, Error } ->
			error_logger:error_msg("Failed to start mesgd_master ~p~n", [ Error ])
	end.
	

connect_master(Master) ->
	case net_kernel:connect(Master) of
		true ->
			error_logger:info_msg("Connected to master ~p~n", [ Master ]),
			[ Host,Domain ] = get_hostname(),
			Addr = get_address(),
			
			rpc:call(Master,mesgd_master,add, Host, Domain, Addr);
		_ ->
			error_logger:error_msg("Failed to connect to master ~p~n", [ Master ]),
			timer:apply_after(5000, ?MODULE, connect_master, [ Master ])
	end.


get_hostname() ->
	Hostname = os:getenv("HOSTNAME"),
	[ Host, Domain | _ ] = string:tokens(Hostname,"."),
	[ Host, Domain ].

get_address() ->
	{ ok, Iface } = application:getenv(mesgd,interface),
	{ ok, Addrs } = inet:getifaddrs(),
	If = proplists:get_value(Iface,Addrs),
	proplists:get_value(addr,If).
	

