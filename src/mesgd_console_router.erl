-module(mesgd_console_router).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2017 David J. Goehrig"/utf8>>).
-export([ response/1, start_link/0, mount/2, mount/1, list/0 ]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

-behavior(gen_server).

-include("include/mesgd_http.hrl").

-record(mesgd_console_router, { routes = [] }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Interface

start_link() ->
	gen_server:start_link({ local, ?MODULE }, ?MODULE, [], []).

%% if we're passed a response pass it along
response(Response = #response{}) ->
	Response;

%% I no claims are passed, don't check
response(Request = #request { method = Method, claims = [] }) ->	
	Content = gen_server:call(?MODULE,{ Method, Request }),
		ContentLength = binary:list_to_bin(integer_to_list(byte_size(Content))),
		#response{ 
			socket = Request#request.socket,
			upgrade = false,
			status = 200,
			protocol = <<"HTTP/1.1">>,
			headers = [
				{ <<"Content-Length">>, ContentLength },
				{ <<"Content-Type">>, <<"text/html">> }
			],
			body = Content,
			claims = []
		};

response(Request = #request { method = Method, path = Path, claims = Claims }) ->	
	case mesgd_auth:check(Path,Claims) of
	invalid ->
		#response{ status = 401 };
	_ ->
		Content = gen_server:call(?MODULE,{ Method, Request }),
		ContentLength = binary:list_to_bin(integer_to_list(byte_size(Content))),
		#response{ 
			socket = Request#request.socket,
			upgrade = false,
			status = 200,
			protocol = <<"HTTP/1.1">>,
			headers = [
				{ <<"Content-Length">>, ContentLength },
				{ <<"Content-Type">>, <<"text/html">> }
			],
			body = Content,
			claims = Claims
		}
	end.

mount({ Path, Module }) ->
	mount(Path,Module).

mount(Path,Module) ->
	gen_server:cast(?MODULE,{ mount, Path, Module }).

list() ->
	gen_server:call(?MODULE, list ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Interface

init([]) ->
	{ ok, #mesgd_console_router{ routes = [] }}.

handle_call({ Method, Request = #request{ method = Method, path = Path }}, _From, Router = #mesgd_console_router{ routes = Routes }) ->
	case mesgd_path:path_scan(Path,Routes) of
		undefined -> 
			{ reply, static_file(Request), Router };
		Module ->
			{ reply, Module:Method(Request), Router }
	end;	

handle_call(list, _From, Router = #mesgd_console_router{ routes = Routes }) ->
	{ reply, Routes, Router };

handle_call(Message,_From,Router) ->
	error_logger:error_msg("Unknown message ~p~n", 	[ Message ]),
	{ reply, ok, Router }.

handle_cast({ mount, Path, Module }, Router = #mesgd_console_router{ routes = Routes }) ->
	error_logger:info_msg("Routing ~p -> ~p~n", [ Path, Module ]),	
	{ noreply, Router#mesgd_console_router { routes = [ { Path, Module } | Routes ] }};

handle_cast(Message,Router) ->
	error_logger:error_msg("Unknown message ~p~n", 	[ Message ]),
	{ noreply, Router }.

handle_info(Message,Router) ->
	error_logger:error_msg("Unknown message ~p~n", 	[ Message ]),
	{ noreply,  Router }.

terminate(Reason,_Router) ->
	error_logger:info_msg("mesgd_console_router shutting down ~p~n", [ Reason ]),
	ok.

code_change(_Old,_Extra,Router) ->
	{ ok, Router }.

file(Path) ->
	{ ok, ConsoleRoot } = mesgd_config:get(console_root),
	Filename = ConsoleRoot ++ Path,
	error_logger:info_msg("Looking for file ~p~n",[ Filename ]),
	file:read_file(Filename).

static_file(#request{ path = Path }) ->
	case file(Path) of
		{ ok, Bin } ->
			Bin;
		{ error, Error } ->
			error_logger:info_msg("Returning empty response ~p~n", [ Error ]),
			<<>>
	end.

