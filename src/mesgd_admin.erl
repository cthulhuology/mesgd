-module(mesgd_admin).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).

-behavior(gen_server).
-export([ start_link/0, stop/0 ]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).


-record(mesgd_admin, {}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
start_link() ->
	gen_server:start_link(?MODULE, #mesgd_admin{}, []).

stop() ->
	gen_server:call(?MODULE,stop).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API

init( Admin = #mesgd_admin{} ) ->
	mesgd_router:connect(self(),"/Admin"),
	{ ok, Admin }.

handle_call(stop,_From,State) ->
	{ stop, stopped, State };

handle_call(Message,_From,State) ->
	error_logger:error_msg("Unknown message ~p~n", [ Message ]),
	{ reply, ok, State }.

handle_cast({ send, Message },State) ->
	error_logger:info_msg("~p~n", [ Message ]),
	admin(Message),
	{ noreply, State };

handle_cast(Message,State) ->
	error_logger:error_msg("Unknown message ~p~n", [ Message ]),
	{ noreply, State }.

handle_info(Message,State) ->
	error_logger:error_msg("Unknown message ~p~n", [ Message ]),
	{ noreply, State }.

terminate(Reason,_State) ->
	error_logger:info_msg("Shutting down mesgd_admin ~p~n", [ Reason ]),
	ok.

code_change(_Old,State,_Extra) ->
	{ ok, State }.

admin(Message) ->
	case proplists:get_value(<<"Admin">>,Message) of
		<<"User">> -> admin_user(Message);
		_ -> ok
	end.

admin_user(Message) ->
	case proplists:get_value(<<"action">>,Message) of
		<<"add">> ->
			Name = proplists:get_value(<<"name">>,Message),
			Email = proplists:get_value(<<"email">>,Message),
			Password = proplists:get_value(<<"password">>,Message),
			case Password of
				<<"">> -> error_logger:info_msg("refusing to set empty password");
				<<"undefined">> -> error_logger:info_msg("refusing to set undefined password");
				_ -> mesgd_auth:add(Name,Email,Password)
			end;
		<<"remove">> ->
			Name = proplists:get_value(<<"name">>,Message),
			Email = proplists:get_value(<<"email">>,Message),
			mesgd_auth:remove(Name,Email);
		<<"list">> ->
			Users = mesgd_auth:users(),
			[ mesgd_router:route([
				{<<"Admin">>,<<"User">>},
				{<<"name">>, Name},
				{<<"email">>, Email}
			]) || { Name, Email } <- Users ];
		_ ->
			ok
	end.
