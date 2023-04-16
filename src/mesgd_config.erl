-module(mesgd_config).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-compile({no_auto_import,[get/1,put/1]}).
-export([ get/1 ]).

get(Key) ->
	case file:consult("mesgd.config") of
		{ ok, Config } -> { ok, proplists:get_value(Key,Config) };
		_ -> { error, nosuchkey }
	end.

