-module(ujson).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).

-export([ encode/1, decode/1 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Methods
%%

encode(false) ->
	<< $f >>;
encode(true) ->
	<< $t >>;
encode(null) ->
	<< $n >>;
encode(<<>>) ->
	<< $s, 0:16/big-unsigned-integer >>;
encode([]) ->
	<< $a, 0:16/big-unsigned-integer >>;
encode([{}]) ->
	<< $o, 0:16/big-unsigned-integer >>;
encode(Atom) when is_atom(Atom) ->
	encode(list_to_binary(atom_to_list(Atom)));
encode(String) when is_binary(String) ->	
	L = size(String),
	<< $s, L:16/big-unsigned-integer, String/binary >>;
encode(Int) when is_integer(Int), Int >= -128, Int < 128 ->
	<< $c, Int:8/signed-integer >>;
encode(Int) when is_integer(Int), Int >= 0, Int < 256 ->
	<< $C, Int:8/unsigned-integer >>;
encode(Int) when is_integer(Int), Int >= -32768, Int < 32768 ->
	<< $w, Int:16/big-integer >>;
encode(Int) when is_integer(Int), Int >= 0, Int < 65536 ->
	<< $W, Int:16/big-unsigned-integer >>;
encode(Int) when is_integer(Int), Int >= -2147483648, Int < 2147483648 ->
	<< $i, Int:32/big-integer >>;
encode(Int) when is_integer(Int), Int >= 0, Int < 4294967296 ->
	<< $I, Int:32/big-unsigned-integer >>;
encode(Int) when is_integer(Int), Int < 0 ->
	<< $q, Int:64/big-integer >>;
encode(Int) when is_integer(Int) ->
	<< $Q, Int:64/big-unsigned-integer >>;
encode(Float) when is_float(Float), Float > 1.0e8 ; Float < -1.0e8 ->
	<< $D, Float:64/big-float >>;
encode(Float) when is_float(Float) ->
	<< $d, Float:32/big-float >>;
encode({K,V}) ->
	Len = size(K),
	Value = encode(V),
	<< Len:16/big-unsigned-integer, K/binary, Value/binary >>;
encode([H|T]) when is_tuple(H) ->
	encode(object, [H|T], <<>>);
encode([H|T]) ->
	encode(array, [H|T], <<>>).

decode(<<>>) ->
	null;
decode(<< $n, Rest/binary>>) ->
	{ null, Rest };
decode(<< $t, Rest/binary>>) ->
	{ true, Rest }; 
decode(<< $f, Rest/binary>>) ->
	{ false, Rest }; 
decode(<< $c, Int:8/signed-integer, Rest/binary>>) ->
	{ Int, Rest };
decode(<< $C, Int:8/unsigned-integer, Rest/binary>>) ->
	{ Int, Rest };
decode(<< $w, Int:16/big-integer, Rest/binary>>) ->
	{ Int, Rest };
decode(<< $W, Int:16/big-unsigned-integer, Rest/binary>>) ->
	{ Int, Rest };
decode(<< $i, Int:32/big-integer, Rest/binary>>) ->
	{ Int, Rest };
decode(<< $I, Int:32/big-unsigned-integer, Rest/binary>>) ->
	{ Int, Rest };
decode(<< $q, Int:64/big-integer, Rest/binary>>) ->
	{ Int, Rest };
decode(<< $Q, Int:64/big-unsigned-integer, Rest/binary>>) ->
	{ Int, Rest };
decode(<< $d, Float:32/big-float, Rest/binary>>) ->
	{ Float, Rest }; 
decode(<< $D, Float:64/big-float, Rest/binary>>) ->
	{ Float, Rest }; 
decode(<< $s, 0:16/big-unsigned-integer, Rest/binary>>) ->
	{ <<>>, Rest };
decode(<< $s, Size:16/big-unsigned-integer, String:Size/binary, Rest/binary >>) ->
	{ String, Rest };
decode(<< $a, 0:16/big-unsigned-integer, Rest/binary >>) ->
	{ [], Rest };
decode(<< $a, Size:16/big-unsigned-integer, Array:Size/binary, Rest/binary >>) ->
	{ decode_array(Array,[]), Rest };
decode(<< $o, 0:16/big-unsigned-integer, Rest/binary >>) ->
	{ [{}], Rest };
decode(<< $o, Size:16/big-unsigned-integer, Object:Size/binary, Rest/binary >>) ->
	{ decode_object(Object,[]), Rest }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Methods
%%

%% encode a list
encode(object,[],Acc) ->
	L = size(Acc),
	<< $o, L:16/big-unsigned-integer, Acc/binary >>;
encode(array,[],Acc) ->
	L = size(Acc),
	<< $a, L:16/big-unsigned-integer, Acc/binary >>;
encode(Type, [H|T],Acc) ->
	Term = encode(H),
	encode(Type, T, << Acc/binary, Term/binary >>).

%% decode an array
decode_array(<<>>, Acc) ->
	lists:reverse(Acc);
decode_array(Array, Acc ) ->
	{ Value, Rem } = decode(Array),
	decode_array(Rem, [ Value | Acc ]).

%% decode and object
decode_object(<<>>, Acc) ->
	lists:reverse(Acc);
decode_object(<<Len:16/big-unsigned-integer, Key:Len/binary, Data/binary>>,Acc) ->
	{ Value, Rem } = decode(Data),
	decode_object(Rem, [ { Key, Value } | Acc ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests
%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

int_test() ->
	?assertMatch(<< $c, 0>>, ujson:encode(0)),
	?assertMatch(<< $c,128>>, ujson:encode(-128)),
	?assertMatch(<< $c, 127:8/integer>>, ujson:encode(127)),
	?assertMatch(<< $C, 128:8/integer>>, ujson:encode(128)),
	?assertMatch(<< $C, 255:8/integer>>, ujson:encode(255)),
	?assertMatch(<< $w, 256:16/big-signed-integer>>, ujson:encode(256)),
	?assertMatch(<< $w, -129:16/big-signed-integer>>, ujson:encode(-129)),
	?assertMatch(<< $w, -32768:16/big-signed-integer>>, ujson:encode(-32768)),
	?assertMatch(<< $W, 32768:16/big-unsigned-integer>>, 
		ujson:encode(32768)),
	?assertMatch(<< $W, 65535:16/big-unsigned-integer>>, 
		ujson:encode(65535)),
	?assertMatch(<< $i, 65536:32/big-signed-integer>>, 
		ujson:encode(65536)),
	?assertMatch(<< $i, -32769:32/big-signed-integer>>, ujson:encode(-32769)),
	?assertMatch(<< $i, -2147483648:32/big-signed-integer>>,
		ujson:encode(-2147483648)),
	?assertMatch(<< $i, 2147483647:32/big-signed-integer>>,
		ujson:encode(2147483647)),
	?assertMatch(<< $I, 2147483648:32/big-unsigned-integer>>,
		ujson:encode(2147483648)),
	?assertMatch(<< $I, 2147483648:32/big-unsigned-integer>>,
		ujson:encode(2147483648)),
	?assertMatch(<< $I, 4294967295:32/big-unsigned-integer>>,
		ujson:encode(4294967295)),
	?assertMatch(<< $q, -4294967296:64/big-signed-integer>>,
		ujson:encode(-4294967296)),
	?assertMatch(<< $Q, 4294967296:64/big-unsigned-integer>>,
		ujson:encode(4294967296)).
-endif.
