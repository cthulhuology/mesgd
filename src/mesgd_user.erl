-module(mesgd_user).
-copyright(<<"© 2017 David J Goehrig, Open Robotics Company LLC."/utf8>>).
-compile({no_auto_import,[get/1]}).

-export([user/1, add/3, remove/1, users/0, details/1, detail/3, install/1 ]).

-include("include/mesgd_auth.hrl").
-include("include/mesgd_user.hrl").

add(User,Email,Password) when is_list(User) ->
	add(list_to_binary(User),Email,Password);
add(User,Email,Password) when is_list(Email) ->
	add(User,list_to_binary(Email),Password);
add(User,Email,Password) when is_list(Password) ->
	add(User,Email,list_to_binary(Password));
add(User,Email,Password) ->
	Auth = base64:encode(<< User/binary,":",Password/binary>>),
	Token = mesgd_auth:tokenize(Auth),
	error_logger:info_msg("Token: ~p~n", [ Token ]),
	F = fun() ->
		remove(User),
		ok = mnesia:write(#mesgd_auth{ token = Token, user = User, email = Email, active = true, paths = [] }),
		error_logger:info_msg("Add User ~p <~p>", [ User, Email ]),
		detail(User,email,Email),
		ok
	end,
	mnesia:activity(transaction,F).

remove(User) when is_list(User) ->
	remove(list_to_binary(User));
remove(User) ->
	F = fun() ->
		case mnesia:match_object(#mesgd_auth{ user = User, email = '_', token = '_', active = '_', paths = '_' }) of
			[] -> ok;
			Records ->
				error_logger:info_msg("Remove User ~p", [ User ]),
				[ mnesia:delete_object(Record) || Record <- Records ]
		end
	end,
	mnesia:activity(transaction,F).

users() ->
	F = fun() ->
		case mnesia:match_object(#mesgd_auth{ user = '_', email = '_', token = '_', active = true, paths = '_' }) of
			Users when is_list(Users) ->
				[ {User,Email} || #mesgd_auth{ user = User, email = Email } <- Users ];
			_ -> []	
		end
	end,
	mnesia:activity(transaction,F).

user(Name) when is_list(Name) ->
	user(list_to_binary(Name));
user(Name) ->
	F = fun() ->
		case mnesia:match_object(#mesgd_auth{ user = Name, email = '_', token = '_', active = true, paths = '_' }) of
			Users when is_list(Users) ->
				[[ { name, User }, { email, Email }, {paths, [ list_to_binary(X) || X <- Paths ] } ] || #mesgd_auth{ user = User, email = Email, paths = Paths } <- Users ];
			_ -> []
		end
	end,
	mnesia:activity(transaction,F).

%% fetches the extended metadata concerning a user by username 
details(User) ->
	F = fun() ->
		case mnesia:read(mesgd_user,{ User }, write) of
			[ #mesgd_user{ uid = { User }, user = User, active = true, properties = Properties } ] ->
				[ { user, User } | Properties ];
			_ ->
				[]
		end
	end,
	mnesia:activity(transaction,F).

%% set an extended bit of metadata on a user 
detail(User,Key,Value) ->
	F = fun() ->
		case mnesia:wread(mesgd_user,{User},write) of
			[#mesgd_user{ uid = { User }, user = User, active = true, properties = Properties } ] ->
				ok = mnesia:write(#mesgd_user{ uid = { User }, user = User, active = true, properties = [ { Key, Value } | proplists:delete(Key,Properties) ] });
			_ ->
				ok = mnesia:write(#mesgd_user{ uid = { User }, user = User, active = true, properties = [ { Key, Value } ] })
		end
	end,
	mnesia:activity(transaction,F).

install(Nodes) ->
	{ atomic, ok } = mnesia:create_table(mesgd_user, [
		{ attributes, record_info(fields,mesgd_user) },
		{ disc_copies, Nodes }]).
