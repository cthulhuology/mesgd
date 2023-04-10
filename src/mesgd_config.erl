-module(mesgd_config).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).
-compile({no_auto_import,[get/1,put/1]}).
-export([ start_link/0, get/2, reload/0, domains/0 ]).
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	terminate/2 ]).


start_link() ->
	gen_server:start_link({ local, ?MODULE }, ?MODULE, [], []).

domains() ->
	gen_server:call(?MODULE,domains).
	
get(Domain,Key) ->
	gen_server:call(?MODULE,{get,Domain,Key}).

reload() ->
	gen_server:call(?MODULE,reload).


init([]) ->
	case file:consult("mesgd.config") of
		{ ok, Config } -> { ok, Config };
		{ _, Reason } -> { stop, Reason }
	end.


handle_call({get,Domain,Key}, _From, Config ) ->
	List = proplists:get_value(Domain,Config),
	Value = proplists:get_value(Key,List),
	{ reply, { ok, Value }, Config };


handle_call(reload,_From,_Config) ->
	case file:consult("mesgd.config") of
		{ ok, NewConfig } -> { noreply, NewConfig };
		{ _, Reason } -> { stop, Reason }
	end;
	
handle_call(Message,_From,State) ->
	error_logger:error_msg("Unknown message ~p~n",[ Message ]),
	{ reply,ok,State }.

handle_cast(Message,State )->
	error_logger:error_msg("Unknown message ~p~n",[ Message ]),
	{ noreply, State }.

handle_info(Message,State )->
	error_logger:error_msg("Unknown message ~p~n",[ Message ]),
	{ noreply, State }.

code_change(_Old,_Extra,Server) ->
	{ ok, Server }.

terminate(_Reason,_Server) ->
	ok.
