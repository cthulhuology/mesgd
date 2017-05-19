-module(mesgd_user).
-copyright(<<"© 2017 David J Goehrig, Open Robotics Company LLC."/utf8>>).
-compile({no_auto_import,[get/1]}).

 -export([user/2, add/4, remove/2, users/1 ]).

-include("include/mesgd_auth.hrl").

add(Domain,User,Email,Password) when is_list(Domain) ->
	add(list_to_binary(Domain),User,Email,Password);
add(Domain,User,Email,Password) when is_list(User) ->
	add(Domain,list_to_binary(User),Email,Password);
add(Domain,User,Email,Password) when is_list(Email) ->
	add(Domain,User,list_to_binary(Email),Password);
add(Domain,User,Email,Password) when is_list(Password) ->
	add(Domain,User,Email,list_to_binary(Password));
add(Domain,User,Email,Password) ->
	Auth = base64:encode(<< User/binary,":",Password/binary>>),
	Token = mesgd_auth:tokenize(Domain,Auth),
	error_logger:info_msg("Token: ~p~n", [ Token ]),
	F = fun() ->
		remove(Domain,User),
		ok = mnesia:write(#mesgd_auth{ token = Token, domain = Domain, user = User, email = Email, active = true, paths = [] }),
		error_logger:info_msg("Add User ~p <~p>", [ User, Email ]),
		ok
	end,
	mnesia:activity(transaction,F).

remove(Domain,User) when is_list(Domain) ->
	remove(list_to_binary(Domain),User);
remove(Domain,User) when is_list(User) ->
	remove(Domain,list_to_binary(User));
remove(Domain,User) ->
	F = fun() ->
		case mnesia:match_object(#mesgd_auth{ domain = Domain, user = User, email = '_', token = '_', active = '_', paths = '_' }) of
			[] -> ok;
			Records ->
				error_logger:info_msg("Remove User ~p", [ User ]),
				[ mnesia:delete_object(Record) || Record <- Records ]
		end
	end,
	mnesia:activity(transaction,F).

users(Domain) when is_list(Domain) ->
	users(list_to_binary(Domain));
users(Domain) ->
	F = fun() ->
		case mnesia:match_object(#mesgd_auth{ domain = Domain, user = '_', email = '_', token = '_', active = true, paths = '_' }) of
			Users when is_list(Users) ->
				[ {User,Email} || #mesgd_auth{ user = User, email = Email } <- Users ];
			_ -> []	
		end
	end,
	mnesia:activity(transaction,F).

user(Domain,Name) when is_list(Domain) ->
	user(list_to_binary(Domain),Name);
user(Domain,Name) when is_list(Name) ->
	user(Domain,list_to_binary(Name));
user(Domain,Name) ->
	F = fun() ->
		case mnesia:match_object(#mesgd_auth{ domain = Domain, user = Name, email = '_', token = '_', active = true, paths = '_' }) of
			Users when is_list(Users) ->
				[[ { name, User }, { email, Email }, {paths, [ list_to_binary(X) || X <- Paths ] } ] || #mesgd_auth{ user = User, email = Email, paths = Paths } <- Users ];
			_ -> []
		end
	end,
	mnesia:activity(transaction,F).
