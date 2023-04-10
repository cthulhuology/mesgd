-module(mesgd_jwt).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start_link/2, stop/0, grant/1, verify/1, revoke/1 ]).
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	terminate/2 ]).

-record(mesgd_jwt, { public, private, revoked }).

-define(SELF,list_to_atom(?MODULE_STRING)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

%% create a mesgd_jwt under supervision for the port
start_link(PublicKeyFile,PrivateKeyFile) ->
	{ ok, PublicKey } = file:read_file(PublicKeyFile),
	{ ok, PrivateKey } = file:read_file(PrivateKeyFile),
	gen_server:start_link({ local, ?SELF }, ?MODULE, #mesgd_jwt{
		public = PublicKey,
		private = PrivateKey,
		revoked = []
		
	}, []).

%% stop the mesgd_jwt server
stop() ->
	gen_server:call(?SELF,stop).

%% grant a set of claims (takes a JSON document)
grant(Claims) ->
	gen_server:call(?SELF,{ grant, Claims }).

%% verifies a jwt token 
verify(Token) ->
	gen_server:call(?SELF,{ verify, Token }).

%% revoke a valid jwt token
revoke(Token) ->
	gen_server:cast(?SELF,{ revoke, Token }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%

is_revoked(Token,Revoked) ->
	lists:any( fun(X) -> X =:= Token end, Revoked).

init(Jwt = #mesgd_jwt{} ) ->
	{ ok, Jwt }.	

handle_call({ grant, Claims },_From, Jwt = #mesgd_jwt{ private = Private }) ->
	Token = jwt:sign(Claims, Private),
	{ reply, {ok, Token}, Jwt };

handle_call({ verify, Token },_From, Jwt = #mesgd_jwt{ public = Public, revoked = Revoked }) ->
	case is_revoked(Token,Revoked) of
		true -> { reply, failed, Jwt };
		_ ->
			case jwt:claims(Token,Public) of
				invalid -> { reply, failed, Jwt };
				Claims -> { reply, { ok, Claims} , Jwt }
			end
	end;

handle_call(Message,_From,Jwt) ->
	error_logger:error_msg("Unknown message ~p", [ Message ]),
	{ reply, ok, Jwt }.

handle_cast({revoke,Token}, Jwt = #mesgd_jwt{ revoked = Revoked }) ->
	{ noreply, Jwt#mesgd_jwt{ revoked = [ Token | Revoked ] }};

handle_cast(Message,Jwt) ->
	error_logger:error_msg("Unknown message ~p", [ Message ]),
	{ noreply, Jwt }.

handle_info(Message,Jwt) ->
	error_logger:error_msg("Unknown message ~p", [ Message ]),
	{ noreply, Jwt }.

code_change(_Old,_Extra,Server) ->
	{ ok, Server }.

terminate(_Reason,_Server) ->
	ok.
