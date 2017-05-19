-module(json).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2066 David J Goehrig"/utf8>>).

-export([ decode/1, encode/1 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Methods

%% decodes json binaries
%%
decode(<<>>) -> 
	<<>>;
decode(<<${, Object/binary>>) ->
	{ Obj, _Rem } = decode_object(Object, []),
	Obj;
decode(<<$[, Array/binary>>) ->
	{ Arr, _Rem } = decode_array(Array, []),
	Arr;
decode(<<$", String/binary>>) ->
	{ Str, _Rem } = decode_string(String, []),
	Str;
decode(<<"true", _Rest/binary>>) ->
	true;
decode(<<"false", _Rest/binary>>) ->
	false;
decode(<<"null", _Rest/binary>>) ->
	null;
decode(<<_:8, Rest/binary>> = Number) ->
	case decode_number(Number,[]) of
		{ Num, _Rem } -> Num;
		_ -> decode(Rest)
	end.

%% encode method, takes a binary and generates an object
%%
encode([]) ->
	<<$[, $]>>;
encode([{}]) ->
	<<${ , $} >>;
encode(<<>>) ->
	<<$", $" >>;
encode(true) ->
	<<"true">>;
encode(false) ->
	<<"false">>;
encode(null) ->
	<<"null">>;
encode(Data) when is_atom(Data) ->
	Bin = binary:list_to_bin("\"" ++ atom_to_list(Data) ++ "\""),
	<<Bin/binary >>;
encode(Data) when is_integer(Data) ->
	Bin = binary:list_to_bin(integer_to_list(Data)),
	<<Bin/binary >>;
encode(Data) when is_float(Data) ->
	Bin = binary:list_to_bin(float_to_list(Data)),
	<<Bin/binary >>;
encode(Data) when is_binary(Data) ->
	Bin  = escape( Data ),
 	<<$", Bin/binary, $" >>;
encode({ Key, Value }) ->
	K = encode(Key),
	V = encode(Value),
	<< K/binary, $:, V/binary >>;
encode(Data) when is_list(Data) and is_tuple(hd(Data)) ->
	Nest = lists:map( fun(X) -> encode(X) end, Data ),
	Obj = join(Nest),
	<<${, Obj/binary, $}>>;
encode( Data) when is_list(Data) ->
	Nest = lists:map( fun(X) -> encode(X) end, Data),
	Arr = join(Nest),	
	<<$[, Arr/binary, $]>>.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Methods
%%

%% decode value
decode_value(<<>>) -> 
	<<>>;
decode_value(<<${, Object/binary>>) ->
	decode_object(Object, []);
decode_value(<<$[, Array/binary>>) ->
	decode_array(Array, []);
decode_value(<<$", String/binary>>) ->
	decode_string(String, []);
decode_value(<<"true", Rest/binary>>) ->
	{ true, Rest };
decode_value(<<"false", Rest/binary>>) ->
	{ false, Rest };
decode_value(<<"null", Rest/binary>>) ->
	{ null, Rest };
decode_value(<<_:8,Rest/binary>> = Number) ->
	case decode_number(Number,[]) of
		{ Num, Rem } -> { Num, Rem };
		_ -> decode_value(Rest)
	end.

%% Decode an object
decode_object(<<$},Rest/binary>>,[]) ->
	{ [{}], Rest };
decode_object(<<$},Rest/binary>>,Acc) ->		% end of object
	{ lists:reverse(Acc), Rest };
decode_object(Object,Acc) -> 
	{ Key, Rem } = decode_key(Object, []),
	Rem2 = decode_colon(Rem),
	{ Val, Rem3 } = decode_value(Rem2),
	Rem4 = decode_comma(Rem3),
	decode_object(Rem4, [ { Key, Val } | Acc ]).

%% Decode an array
decode_array(<<$], Rest/binary>>, Acc) ->
	{ lists:reverse(Acc), Rest };
decode_array(Array,Acc) ->
	{ Val, Rem } = decode_value(Array),
	Rem2 = decode_comma(Rem),
	decode_array(Rem2, [ Val | Acc ]).

%% Decode a string
decode_string(<<>>,[]) ->
	{ <<>>, <<>> };
decode_string(<<$", Rest/binary>>, Acc) ->
	{ binary:list_to_bin(lists:reverse(Acc)), Rest };
decode_string(<<$\\ , $", String/binary>>, Acc) ->
	decode_string(String, [ $" | Acc ]);	
decode_string(<<X:8, String/binary>>, Acc ) ->
	decode_string(String, [ X | Acc ]).

decode_key(<<$", String/binary>>, Acc) ->
	decode_string(String,Acc);
decode_key(<<_:8, String/binary>>, Acc) ->
	decode_key(String,Acc).
	
%% parse out a colon
decode_colon(<<8, Rest/binary>>) ->
	decode_colon(Rest);	
decode_colon(<<10, Rest/binary>>) ->
	decode_colon(Rest);	
decode_colon(<<13, Rest/binary>>) ->
	decode_colon(Rest);	
decode_colon(<<32, Rest/binary>>) ->
	decode_colon(Rest);	
decode_colon(<<$:, Rest/binary>>) ->
	decode_colon(Rest);	
decode_colon(<<Rem/binary>>) ->
	Rem.

%% parse out a comma
decode_comma(<<8, Rest/binary>>) ->
	decode_comma(Rest);	
decode_comma(<<10, Rest/binary>>) ->
	decode_comma(Rest);	
decode_comma(<<13, Rest/binary>>) ->
	decode_comma(Rest);	
decode_comma(<<32, Rest/binary>>) ->
	decode_comma(Rest);	
decode_comma(<<$,, Rest/binary>>) ->
	decode_comma(Rest);	
decode_comma(<<Rem/binary>>) ->
	Rem.

decode_number(<<$0, Rest/binary>>, Acc ) ->
	decode_number(Rest, [ $0 | Acc]);
decode_number(<<$1, Rest/binary>>, Acc ) ->
	decode_number(Rest, [ $1 | Acc ]);
decode_number(<<$2, Rest/binary>>, Acc ) ->
	decode_number(Rest, [ $2 | Acc]);
decode_number(<<$3, Rest/binary>>, Acc ) ->
	decode_number(Rest, [ $3 | Acc]);
decode_number(<<$4, Rest/binary>>, Acc ) ->
	decode_number(Rest, [ $4 | Acc]);
decode_number(<<$5, Rest/binary>>, Acc ) ->
	decode_number(Rest, [ $5 | Acc]);
decode_number(<<$6, Rest/binary>>, Acc ) ->
	decode_number(Rest, [ $6 | Acc]);
decode_number(<<$7, Rest/binary>>, Acc ) ->
	decode_number(Rest, [ $7 | Acc]);
decode_number(<<$8, Rest/binary>>, Acc ) ->
	decode_number(Rest, [ $8 | Acc]);
decode_number(<<$9, Rest/binary>>, Acc ) ->
	decode_number(Rest, [ $9 | Acc]);
decode_number(<<$-, Rest/binary>>, Acc ) ->
	decode_number(Rest, [ $- | Acc]);
decode_number(<<$+, Rest/binary>>, Acc ) ->
	decode_number(Rest, [ $+ | Acc ]);
decode_number(<<$., Rest/binary>>, Acc ) ->
	decode_number(Rest, [ $. | Acc ]);
decode_number(<<$e, Rest/binary>>, Acc ) ->
	decode_number(Rest, [ $e | Acc ]);
decode_number(<<$E, Rest/binary>>, Acc ) ->
	decode_number(Rest, [ $E | Acc ]);
decode_number(Rem,Acc) ->
	case to_number(lists:reverse(Acc)) of
		false -> false;
		Num -> { Num , Rem }
	end.
	
to_number([]) ->
	false;	
to_number(Acc) ->
	try list_to_float(Acc)
	catch error:badarg -> 
		try 
			list_to_integer(Acc) 
		catch error:badarg ->
			false
		end
	end.

escape( Bin ) when is_binary(Bin) ->
	escape( Bin, <<>> ).

escape( <<>>, Acc ) ->
	Acc;
escape( <<$\\ , Rest/binary >>, Acc ) ->
	escape(Rest, << Acc/binary, $\\ , $\\ >>);
escape( <<$", Rest/binary >>, Acc ) ->
	escape(Rest, << Acc/binary ,  $\\ , $" >>);
escape( <<X:8,Rest/binary>>, Acc ) ->
	escape( Rest, << Acc/binary, X >> ).

join(List) ->
	join(List,<<>>).

join([], Acc) ->
	Acc;
join([ A, B | Rest ], Acc)  when is_binary(A) and is_binary(B) ->
	join( [ B | Rest ], << Acc/binary, A/binary, $, >>);
join([ B ], Acc) when is_binary(B) ->
	<< Acc/binary, B/binary >>.
	
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

empty_object_test() ->
	?assertEqual( [{}], json:decode(<<"{}\r\n">>)),
	?assertEqual( [ [{}], [{}] ],
		json:decode(<<" [ {} , {} ] ">>)).

empty_array_test() ->
	?assertEqual( [], json:decode(<<"[]\r\n">>)).

empty_string_test() ->
	?assertEqual( <<>>, json:decode(<<"\"\"\r\n">>)).

true_test() ->
	?assertEqual( true, json:decode(<<"true\r\n">>)).

false_test() ->
	?assertEqual( false, json:decode(<<"false\r\n">>)).

null_test() ->
	?assertEqual( null, json:decode(<<"null\r\n">>)).

int_test() ->
	?assertEqual( 123, json:decode(<<"  123 \r\n">>)).

float_test() ->
	?assertEqual( 1.23, json:decode(<<"    1.23 \r\n">>)).

array_test() ->
	?assertEqual( [ <<"foo">>, 123, 1.23 ], json:decode(<<" [ \"foo\", 123, 1.23 ] \r\n">>)).

nested_array_test() ->
	?assertEqual([ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ]],
		json:decode(<<"[[1,2,3],[4,5,6],[7,8,9]]\r\n">>)),
	?assertEqual([ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ]],
		json:decode(<<"[[1,2,3],\r\n[4,5,6],\r\n[7,8,9]]\r\n">>)).

object_test() ->
	?assertEqual([{<<"foobar">>, <<"narf">>}, { <<"x">>, 123}, { <<"y">>, 1.23}, {<<"z">>, null}], json:decode(<<"   { \"foobar\" : \"narf\", \"x\" : 123, \"y\" : 1.23, \"z\" : null } \r\n">>)),
	?assertEqual([ { <<"points">>, [[ { <<"x">>, 1 }, {<<"y">>, 2}, {<<"z">>, 3} ],
		[ { <<"x">>, 4 }, {<<"y">>, 5}, {<<"z">>, 6 } ]]}, { <<"name">>, <<"poly">> }],
		json:decode(<<"  { \"points\": [ { \"x\": 1, \"y\" : 2, \"z\": 3 }, { \"x\": 4, \"y\": 5, \"z\" : 6 }], \"name\" : \"poly\" }  \r\n">>)).

string_test() ->
	?assertEqual( <<"foobar">>, json:decode(<<"\"foobar\"">>)),
	?assertEqual( <<"foo\"bar">>, json:decode(<<"\"foo\\\"bar\"">>)),
	?assertEqual( <<"foobar">>, json:decode(<<"   \"foobar\"   ">>)),
	?assertEqual( <<"    foobar    ">>, json:decode(<<"\"    foobar    \"   ">>)).

-endif.
