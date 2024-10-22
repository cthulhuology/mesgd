-module(mesgd_http).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-export([ 
	start/2,stop/1,
	request/2, response/1,
	contains_blank_line/1,
	dump/1,
	loop/0
 ]).

-include("../include/mesgd_http.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

start(_StartType,_StartArgs) ->
	{ ok, spawn(http,loop,[]) }.

stop(_State) ->
	ok.

request(Http = #request{ stage = request_line, data = Seen }, NewData ) ->
	Data = <<Seen/binary,NewData/binary>>,
	case contains_endof_line(Data) of
		false ->
			Http#request{ data = Data };
		_EndOfRequestLine ->
			[ {X,_L} | _M ] = binary:matches(Data,<<"\r\n">>),
			[ Method,Path,Protocol] = string:tokens(binary:bin_to_list(binary:part(Data,0,X))," "),
			request(Http#request{
				stage = parse_headers,
				data = Data,
				method = list_to_atom(string:to_lower(Method)),
				path = Path,
				protocol = Protocol
			},<<>>)
	end;

request(Http = #request{ stage = parse_headers, data = Seen }, NewData ) ->
	Data = <<Seen/binary,NewData/binary>>,
	case contains_blank_line(Data) of
		false ->
			Http#request{ data = Data };
		EndOfHeaders -> 
			[ { X,L} |Matches] = binary:matches(Data,<<"\r\n">>),
			Headers = parse_headers(Data,Matches,X+L,[]),
			request(Http#request{ 
				stage = done,
				data = data,
				headers = Headers, 
				body = binary:part(Data,EndOfHeaders,byte_size(Data) - EndOfHeaders)
			},<<>>)
	end;


request(Http = #request{ stage = done, body = Body }, NewData) ->
	Http#request{
		body = <<Body/binary,NewData/binary>>
	};

request(Http = #request{ stage = _ }, NewData) ->
	error_logger:error_msg("unknown http protocol stage, got data ~p", [ NewData ]),
	Http.

response(#response{ status = Code, headers = Headers, body = Body}) ->
	Reason = status(Code),
	Header = lists:foldr(fun({K,V},B) -> 
		<< K/binary, ": ", V/binary, "\r\n", B/binary >> 
	end, <<>>, Headers),
	Status = integer_to_binary(Code),
	<< "HTTP/1.1 ", Status/binary, " ", Reason/binary, "\r\n", Header/binary, "\r\n", Body/binary >>.

%% determines if we have reached a end of line character
contains_endof_line(Data) when is_binary( Data ) ->
	case binary:matches(Data,<<"\n">>) of
		[] -> false;
		[{Offset,1}|_M] -> Offset+1
	end.

%% determines if we reached the end of the HTTP headers
contains_blank_line( Data ) when is_binary( Data ) ->
	case binary:matches(Data,<<"\r\n\r\n">>) of
		[] -> case binary:matches(Data,<<"\n\n">>) of
				[] -> false;	
				[{Offset,2}|_M] -> Offset+2
			end;
		[{Offset,4}|_M] -> Offset+4	
	end.

%% Process HTTP headers for deciding which Protocol Version to use
parse_headers( _Data, [], _Offset, Acc ) ->
	Acc;
parse_headers( _Data, [{ Offset, _L}| _Matches], Offset, Acc ) ->
	Acc;
parse_headers( Data, [{X,L}|Matches], Offset, Acc ) ->
	Bin = binary:part(Data,Offset,X-Offset), 
	case binary:split(Bin,<<": ">>) of
		[ Key, Value ]  ->
		  	parse_headers(Data, Matches, X+L, [{ Key, Value }|Acc]);
		_ -> Acc
	end.
		

dump(#request{ method = Method, path = Path, protocol = Protocol, headers = Headers, body = Body }) ->
	error_logger:info_msg("method: ~p~npath: ~p~nprotocol: ~p~nheaders: ~p~nbody:~n~p~n", [ Method,Path,Protocol,Headers,Body ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%

status(100) -> <<"Continue">>;
status(101) -> <<"Switching Protocols">>;
status(200) -> <<"OK">>;
status(201) -> <<"Created">>;
status(202) -> <<"Accepted">>;
status(203) -> <<"No-Authoritative Information">>;
status(204) -> <<"No Content">>;
status(205) -> <<"Reset Content">>;
status(206) -> <<"Partial Content">>;
status(300) -> <<"Multiple Choices">>;
status(301) -> <<"Moved Permanently">>;
status(302) -> <<"Found">>;
status(303) -> <<"See Other">>;
status(304) -> <<"Not Modified">>;
status(305) -> <<"Use Proxy">>;
status(400) -> <<"Bad Request">>;
status(401) -> <<"Unauthorized">>;
status(402) -> <<"Payment Required">>;
status(403) -> <<"Forbidden">>;
status(404) -> <<"Not Found">>;
status(405) -> <<"Method Not Allowed">>;
status(406) -> <<"Not Acceptable">>;
status(407) -> <<"Proxy Authentication Required">>;
status(408) -> <<"Request Time-out">>;
status(409) -> <<"Conflict">>;
status(410) -> <<"Gone">>;
status(411) -> <<"Length Required">>;
status(412) -> <<"Precondition Failed">>;
status(413) -> <<"Request Entity Too Large">>;
status(414) -> <<"Request-URI Too Large">>;
status(415) -> <<"Unsupported Media Type">>;
status(416) -> <<"Request Range not satisfiable">>;
status(417) -> <<"Expectation Failed">>;
status(500) -> <<"Internal Server Error">>;
status(501) -> <<"Not Implemented">>;
status(502) -> <<"Bad Gateway">>;
status(503) -> <<"Service Unavailable">>;
status(504) -> <<"Gateway Time-out">>;
status(505) -> <<"HTTP Version not supported">>.

loop() ->
	loop().
