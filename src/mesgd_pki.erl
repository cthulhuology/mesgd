-module(mesgd_pki).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2024 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start_link/0, stop/0, add_public/2, add_private/2, lookup/1, 
	to_message/2, from_message/2,
	message_to/2, message_from/2,
	generate/1, load/2 
	 ]).
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	terminate/2 ]).

-record(jwk, { kty, n, e, alg, kid, use, rsa }).
-record(mesgd_pki, { private = [], public = [], keystore = "./keys" }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

start_link() ->
	gen_server:start_link({ local, ?MODULE }, ?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, stop).

add_public(Name,Key) ->
	gen_server:cast(?MODULE, { add_public, Name, Key }).

add_private(Name,Key) ->
	gen_server:cast(?MODULE, { add_private, Name, Key }).

lookup(Name) ->
	gen_server:call(?MODULE, { lookup, Name }).

to_message(Name,Data) ->
	gen_server:call(?MODULE, { to_message, Name, Data }).

from_message(Name,Data) ->
	gen_server:call(?MODULE, { from_message, Name, Data }).

message_to(Name,Data) ->
	gen_server:call(?MODULE, { message_to, Name, Data }).

message_from(Name,Data) ->
	gen_server:call(?MODULE, { message_from, Name, Data }).

generate(Name) ->
	gen_server:call(?MODULE, { generate, Name }).

load(Name,File) ->
	gen_server:call(?MODULE, { load, Name, File }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%

init([]) ->
	{ ok, KeyStore } = mesgd_config:get(keystore),
	{ ok, #mesgd_pki{ keystore = KeyStore } }.

handle_call({ lookup, Name }, _From, State = #mesgd_pki{ public = Public }) ->
	{ ok, Key } = proplists:get_value(Name,Public),
	{ reply, Key, State };

handle_call({ to_message, Name, Data }, _From, State = #mesgd_pki{ public = Public }) ->
	{ ok, #jwk{ rsa=Key } } = proplists:get_value(Name,Public),
	Message = public_key:decrypt_public(Data,Key),
	{ reply, Message, State };
	
handle_call({ from_message, Name, Data }, _From, State = #mesgd_pki{ private = Private }) ->
	{ ok, #jwk{ rsa=Key } } = proplists:get_value(Name,Private),
	Message = public_key:decrypt_private(Data,Key),
	{ reply, Message, State };

handle_call({ message_to, Name, Message }, _From, State = #mesgd_pki{ public = Public }) ->
	{ ok, #jwk{ rsa=Key } } = proplists:get_value(Name,Public),
	Data = public_key:encrypt_public(Message,Key),
	{ reply, Data, State };

handle_call({ message_from, Name, Message }, _From, State = #mesgd_pki{ private = Private }) ->
	{ ok, #jwk{ rsa=Key } } = proplists:get_value(Name,Private),
	Data = public_key:encrypt_private(Message,Key),
	{ reply, Data, State };

handle_call({ generate, Name }, _From, State = #mesgd_pki{ private = Private, public = Public }) ->
	{ PrivateJWK, PublicJWK } = jwk:keypair(Name),
	{ reply, { PrivateJWK, PublicJWK }, State#mesgd_pki{ 
		private = [ { Name, PrivateJWK } || Private ],
		public =  [ { Name, PublicJWK } || Public ] }};

handle_call({ load, Name, File }, _From, State = #mesgd_pki{ private = Private, public = Public, keystore = KeyStore }) ->
	PrivateJWK  = jwk:private(Name, KeyStore ++ "/private/" ++ File),
	PublicJWK =jwk:public(Name, KeyStore ++ "/public/" ++ File),
	{ reply, { PrivateJWK, PublicJWK }, State#mesgd_pki{
		private = [ { Name, PrivateJWK } || Private ],
		public =  [ { Name, PublicJWK } || Public ] }};

handle_call(Message, _From, State ) ->
	error_logger:error_msg("Unknown message ~p", [ Message ]),
	{ reply, ok, State }.

handle_cast({ add_public, Name, Key }, State = #mesgd_pki{ public = Public }) ->
	{ noreply, State#mesgd_pki{ public = [ { Name, Key } || Public ] }};

handle_cast({ add_private, Name, Key }, State = #mesgd_pki{ private = Private }) ->
	{ noreply, State#mesgd_pki{ private = [ { Name, Key } || Private ] }};

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

