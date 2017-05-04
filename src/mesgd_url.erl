-module(mesgd_url).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).

-export([ encode/1, decode/1 ]).

decode([], Acc) ->
	lists:reverse(Acc);
decode([ $%, X, Y | T ], Acc) ->
	decode( T, [ list_to_integer([X,Y],16) | Acc ]);
decode([ $+ | T ], Acc) ->
	decode(T, [ 32 | Acc ]); 
decode([ X | T ], Acc ) ->
	decode(T, [ X | Acc ]).

decode(List) ->
	decode(List,[]).

encode([], Acc )->
	lists:reverse(Acc);
encode([ X | T ], Acc) ->
	case unreserved(X) of
		true -> encode(T, [ X | Acc ]);
		false -> 
			[ A, B ] = integer_to_list(X,16),
			encode(T,  [ B, A, $% |  Acc ])
	end. 

encode(List) ->
	encode(List,[]).

unreserved($-) -> true;
unreserved($.) -> true;
unreserved($_) -> true;
unreserved($~) -> true;
unreserved(X) ->
	(($0 =< X) and (X =< $9)) or
	(($a =< X) and (X =< $z)) or
	(($A =< X) and (X =< $Z)).


