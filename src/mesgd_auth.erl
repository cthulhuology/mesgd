-module(mesgd_auth).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2023 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start_link/0, auth/1, check/2, forge/1 ]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2 ]).


-include("include/mesgd_http.hrl").

start_link() ->
	gen_server:start_link({ local, ?MODULE }, ?MODULE, [], []).

%% Public
auth(Request) ->
	gen_server:call(?MODULE,{auth,Request}).

check(Path,Claims) ->
	gen_server:call(?MODULE,{check,Path,Claims}).

forge(Claims) ->
	gen_server:call(?MODULE,{forge,Claims}).

%% Private

init([]) ->
	{ ok, [] }.

handle_call({ check, Path, Token }, _From, State ) ->
	Auth =  validate(Path,Token),
	{ reply, Auth, State };

handle_call({ auth, #request{ path = Path, headers = Headers } }, _From, State ) ->
	case proplists:get_value(<<"Authorization">>,Headers) of
	undefined ->
		case proplists:get_value(<<"Sec-WebSocket-Protocol">>,Headers) of
		undefined  ->	
			{ reply, [], State };	%% no auth and no websocket
		<<"json, ", Token/binary>> ->
			io:format("Found Token ~p~n", [ Token ]),
			Auth = validate(Path,Token),
			{ reply, Auth, State };
		<<"ujson, ", Token/binary>> ->
			io:format("Found Token ~p~n", [ Token ]),
			Auth = validate(Path,Token),
			{ reply, Auth, State }
		end;
	<<"Bearer ",Token/binary>> ->
		io:format("got token ~p~n", [ Token ]),
		Auth = validate(Path,Token),
		{ reply, Auth, State };	
	<<"Basic ",Pword/binary>> -> 
		io:format("got password ~p~n", [ Pword ]),
		Token = base64:decode(Pword),	
		Auth = validate(Path,Token),
		{ reply, Auth, State }
	end;

handle_call({forge, Claims}, _From, State ) ->
	JSON = pairwise(Claims),
	{ ok, JWT } = mesgd_jwt:grant(JSON),
	{ reply, JWT, State };
		
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

validate(Path,Token) ->
	case mesgd_jwt:verify(Token) of
		invalid -> invalid;
		{ ok, Claims} ->
			io:format("Found claims ~p comparing with path ~p~n", [ Claims, Path ]),
			case mesgd_path:match(Claims,Path) of
			true ->
				io:format("Path matches claims"),
				Claims;
			_ ->
				io:format("Path does not match claims"),
				invalid
			end
	end.

pairwise([K,V|T], Acc) ->
	pairwise(T,[{K,V}|Acc]);
pairwise([K],Acc) ->
	[{K,true}|Acc];
pairwise([],Acc) ->
	Acc.

pairwise(L) when is_list(L) ->
	pairwise(L,[]).
