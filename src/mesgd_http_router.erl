-module(mesgd_http_router).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2017 David J. Goehrig"/utf8>>).
-export([ response/2, start_link/1, mount/3, mount/2, reload/1, list/1 ]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

-behavior(gen_server).

-include("include/mesgd_http.hrl").

-record(mesgd_http_router, { domain, routes = [] }).

-define(SELF, list_to_atom("mesgd_http_router_" ++ binary:bin_to_list(Domain))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Interface

start_link(Domain) ->
	gen_server:start_link({ local, ?SELF }, ?MODULE, [Domain], []).

%% if we're passed a response pass it along
response(_Domain,Response = #response{}) ->
	Response;

response(Domain,Request = #request { method = Method }) ->	
	Content = gen_server:call(?SELF,{ Method, Request }),
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
		body = Content
	}.

mount(Domain,{ Path, Module }) ->
	mount(Domain,Path,Module).

mount(Domain,Path,Module) ->
	gen_server:cast(?SELF,{ mount, Path, Module }).

reload(Domain) ->
	gen_server:cast(?SELF, reload ).

list(Domain) ->
	gen_server:call(?SELF, list ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Interface

init([Domain]) ->
	reload(Domain),
	{ ok, #mesgd_http_router{ domain = Domain, routes = [] }}.

handle_call({ Method, Request = #request{ method = Method, path = Path }}, _From, Router = #mesgd_http_router{ domain = Domain, routes = Routes }) ->
	case mesgd_path:path_scan(Path,Routes) of
		undefined -> 
			{ reply, static_file(Domain,Request), Router };
		Module ->
			{ reply, Module:Method(Request), Router }
	end;	

handle_call(list, _From, Router = #mesgd_http_router{ routes = Routes }) ->
	{ reply, Routes, Router };

handle_call(Message,_From,Router) ->
	error_logger:error_msg("Unknown message ~p~n", 	[ Message ]),
	{ reply, ok, Router }.

handle_cast(reload, Router) ->
	Directory = mesgd_path:priv(),
	ModuleDir = Directory ++ "/modules/",
	{ ok, Dir } = file:list_dir(ModuleDir),
	Files = lists:usort(lists:map ( fun (F) -> lists:nth(1,string:tokens(F,".")) end, Dir)),
	Modules = [ code:load_abs(ModuleDir ++ File) || File <- Files ],
	error_logger:info_msg("Loaded ~p~n", [ Modules ]),
	[ code:purge(Module) || { module, Module } <- Modules ],
	Routes = [ Module:init() || { module, Module } <- Modules ],
	error_logger:info_msg("Modules: ~p~n", [ Routes ]),
	{ noreply, Router#mesgd_http_router{ routes = Routes }};

handle_cast({ mount, Path, Module }, Router = #mesgd_http_router{ routes = Routes }) ->
	error_logger:info_msg("Routing ~p -> ~p~n", [ Path, Module ]),	
	{ noreply, Router#mesgd_http_router { routes = [ { Path, Module } | Routes ] }};

handle_cast(Message,Router) ->
	error_logger:error_msg("Unknown message ~p~n", 	[ Message ]),
	{ noreply, Router }.

handle_info(Message,Router) ->
	error_logger:error_msg("Unknown message ~p~n", 	[ Message ]),
	{ noreply,  Router }.

terminate(Reason,_Router) ->
	error_logger:info_msg("mesgd_http_router shutting down ~p~n", [ Reason ]),
	ok.

code_change(_Old,_Extra,Router) ->
	{ ok, Router }.

default_file(Domain, #request{ path = Path }) ->
	PathBin = binary:list_to_bin(Path),
	<<"<script>ws = new WebSocket('wss://", Domain/binary, PathBin/binary,
		"','json'); ws.onmessage = function(msg) {"
		" console.log(JSON.parse(msg.data)) };</script>">>.

file(Domain,Path) ->
	Filename = mesgd_path:priv() ++ "/html/" ++ binary:bin_to_list(Domain) ++ Path,
	error_logger:info_msg("Looking for file ~p~n",[ Filename ]),
	file:read_file(Filename).

static_file(Domain, Request = #request{ path = Path }) ->
	case file(Domain,Path) of
		{ ok, Bin } ->
			Bin;
		{ error, Error } ->
			error_logger:info_msg("Returning default ~p~n", [ Error ]),
			default_file(Domain,Request)
	end.

