-module(mesgd_auth).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2023 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start_link/0, auth/1, check/2 ]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2 ]).


-include("include/mesgd_http.hrl").

start_link() ->
	gen_server:start_link({ local, ?MODULE }, ?MODULE, [], []).

%% Public
auth(Request) ->
	gen_server:call(?MODULE,{auth,Request}).

check(Method,Claims) ->
	gen_server:call(?MODULE,{check,Method,Claims}).

%% Private

init([]) ->
	{ ok, [] }.

handle_call({ check, _Method, _Claims }, _From, State ) ->
	{ reply, authorized, State };

handle_call({ auth, #request{ headers = Headers } }, _From, State ) ->
	case proplists:get_value(<<"Authorization">>,Headers) of
	undefined -> { reply, {ok, []}, State };
	Auth -> 
		[ <<"Basic">>,Pword] = string:split(Auth," "),
		io:format("got password ~p~n", [ Pword ]),
		JWT = base64:decode(Pword),	
		io:format("Got JWT ~p~n", [ JWT ]),
		case mesgd_jwt:verify(JWT) of
			invalid -> { reply, invalid, State };
			{ ok, Claims} ->
				io:format("Found claims ~p~n", [ Claims ]),
				{ reply, Claims, State }
		end
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
