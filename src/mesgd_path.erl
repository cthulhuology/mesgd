-module(mesgd_path).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016,2017 David J Goehrig, Open Robotics Company LLC."/utf8>>).

-export([ match/2, scan/2, components/1, eval/2, validate/2, priv/0  ]).


priv() ->
	case code:priv_dir(mesgd) of
		{ error,bad_name } ->
			{ ok, Dir} = application:get_env(mesgd,path),
			Dir;
		Dir ->
			Dir
	end.

segments([K,V]) ->
	{ K, V };
segments([K]) ->
	{ K, "*" };
segments(_) ->
	{}.

components(Path) ->
	Segments = string:tokens(Path,"/"),
	[ segments(string:tokens(Segment,"=")) || Segment <- Segments ].

match(Data,Path) ->
	Pattern = components(Path), 
	compare(Data,Pattern).

%% content sensitive matching
compare(_Data,[]) -> 
	true;	
compare(_Data,[{}]) -> 
	true;
compare(_Data,[ { "*", "*"} | _Tail ]) -> 
	true;
compare(Data, [ { K, "*" } | Tail ]) when is_list(Data) ->
	case lists:member(binary:list_to_bin(K),proplists:get_keys(Data)) of
		true ->	
			compare(Data, Tail );
		_ -> 
			false
	end;
compare(Data, [ { K, V } | Tail]) when is_list(Data) ->
	case integer_key(K) of
		false ->
			case lists:member({binary:list_to_bin(K),binary:list_to_bin(V)},Data) of
				true -> compare(Data,Tail);
				_ -> false
			end;
		N ->
			case N < length(Data) of
				true ->
					case lists:nth(N+1,Data) =:= binary:list_to_bin(V) of
						true -> compare(Data,Tail);
						_ -> false
					end;
				_ ->
					false
			end
	end;

compare(_Data, [ { _K, _V } | _Tail ]) ->
	false.

integer_key(K) ->
	case string:to_integer(K) of
		{ N, []} -> N;
		_ -> false
	end.
	
%% searches a proplist for a path match
%% { Pattern, Pid }
scan(Data, Paths) ->
	[ V || { V, _ } <- lists:filter( fun({ _Pid, Path }) ->
		M = match(Data,Path),
		error_logger:info_msg("Path: ~p Matches: ~p Data: ~p~n", [ Path, M, Data ]),
		M
	 end, 
	Paths) ].

%% compares a path vs a path pattern
eval(Path,Pattern) ->
	error_logger:info_msg("~p vs ~p~n", [Path, Pattern]),
	PathSegments = components(Path),
	PatternSegments = components(Pattern),
	validate(PathSegments,PatternSegments).

validate([],[]) ->
	error_logger:info_msg("validate true~n"),
	true;
validate([{KA,VA}|TA],[{KB,VB}|TB]) ->
	error_logger:info_msg("A ~p vs B ~p~n (~p / ~p )", [ KA, KB, TA,TB]),
	case (KB =:= "*") 
		or ((KA =:= KB) 
			and ((VA =:= VB) 
			or (VB =:= "*"))) of
		true -> validate(TA,TB);
		false -> false
	end;
validate([],[{"*","*"}|_TB]) ->
	true;
validate(_,[]) ->
	true;
validate(_,_) ->
	error_logger:info_msg("validate false~n"),
	false.
	
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
match_test() ->
	?assertEqual(true, mesgd_path:match( [ <<"foo">>, <<"bar">>, <<"narf">>, <<"blat">> ], "/0=foo" )),
	?assertEqual(false, mesgd_path:match( [ <<"foo">>, <<"bar">>, <<"narf">>, <<"blat">> ], "/5=foo" )),
	?assertEqual(true, mesgd_path:match( [ <<"foo">>, <<"bar">>, <<"narf">>, <<"blat">> ], "/1=bar/3=blat" )),
	?assertEqual(true, mesgd_path:match( [ {<<"foo">>,<<"bar">>},{<<"narf">>,<<"blat">>}], "/")),
	?assertEqual(true, mesgd_path:match( [ {<<"foo">>,<<"bar">>},{<<"narf">>,<<"blat">>}], "/*")),
	?assertEqual(true, mesgd_path:match( [ {<<"foo">>,<<"bar">>},{<<"narf">>,<<"blat">>}], "/foo/*")),
	?assertEqual(true, mesgd_path:match( [ {<<"foo">>,<<"bar">>},{<<"narf">>,<<"blat">>}], "/foo/narf")),
	?assertEqual(false, mesgd_path:match( [ {<<"foo">>,<<"bar">>},{<<"narf">>,<<"blat">>}], "/foo/narf/blat")),
	?assertEqual(true, mesgd_path:match( [ {<<"foo">>,<<"bar">>},{<<"narf">>,<<"blat">>}], "/foo/narf=blat")),
	?assertEqual(false, mesgd_path:match( [ {<<"foo">>,<<"bar">>},{<<"narf">>,<<"blat">>}], "/foo=blat/narf=blat")),
	?assertEqual(true, mesgd_path:match( [ {<<"foo">>,<<"bar">>},{<<"narf">>,<<"blat">>}], "/foo=bar/narf=")),
	?assertEqual(true, mesgd_path:match( [ {<<"foo">>,<<"bar">>},{<<"narf">>,<<"blat">>}], "/foo=bar/narf=*")),
	?assertEqual(true, mesgd_path:match( [ {<<"foo">>,<<"bar">>},{<<"narf">>,<<"blat">>}], "/foo=bar/narf")),
	?assertEqual(true, mesgd_path:match( [ {<<"foo">>,<<"bar">>},{<<"narf">>,<<"blat">>}], "/foo=bar/*/narf=borf")).
-endif.

