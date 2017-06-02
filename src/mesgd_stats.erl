-module(mesgd_stats).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2017 David J Goehrig"/utf8>>).
-behavior(gen_server).

-include("../include/mesgd_stats.hrl").

-export([start_link/3, install/1, record/1, sample/0, store/0 ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% start a stats server, sample every Sample seconds, and store every Store seconds usually 1,60 
start_link(Master, Sample,Store) ->
	gen_server:start_link({ local, ?MODULE  }, ?MODULE, [ Master, Sample, Store ], [] ).

record(Event) ->
	gen_server:cast(?MODULE, { record, Event }).

sample() ->
	gen_server:cast(?MODULE, sample ).

store() ->
	gen_server:cast(?MODULE, store ).

init([Master, Sample,Store]) ->
	{ ok, SampleTimer } = timer:apply_interval(Sample*1000,?MODULE,sample,[]),
	{ ok, StoreTimer } = timer:apply_interval(Store*1000,?MODULE,store,[]),
	{ ok, { SampleTimer, StoreTimer, #mesgd_stats{ 
		master = Master,
		time = erlang:system_time(), 
		duration = Store,
		samples = Store / Sample,
		http_in = [0],
		http_out = [0],
		data_in = [0],
		data_out = [0],
		msgs_in = [0],
		msgs_out = [0],
		auth_ack = [0],
		auth_nak = [0]
	}}}.

handle_call(Message,_From, State) ->
	error_logger:error_msg("Unknown message ~p~n", [ Message ]),
	{ reply, ok, State }.

handle_cast({record, Event}, { T1, T2, Stats = #mesgd_stats{
	http_in = HttpIn, http_out = HttpOut, data_in = DataIn, data_out = DataOut, msgs_in = MsgsIn, msgs_out = MsgsOut,
	auth_ack = AuthAck, auth_nak = AuthNack }}) ->
	HttpIn2 = update(http_in,HttpIn,Event),
	HttpOut2 = update(http_out,HttpOut,Event),
	DataIn2 = update(data_in,DataIn,Event),
	DataOut2 = update(data_out,DataOut,Event),
	MsgsIn2 = update(msgs_in,MsgsIn,Event),
	MsgsOut2 = update(msgs_out,MsgsOut,Event),
	AuthAck2 = update(auth_ack,AuthAck,Event),
	AuthNack2 = update(auth_nak,AuthNack,Event),
	{ noreply, {T1,T2,Stats#mesgd_stats{
		http_in = HttpIn2,
		http_out = HttpOut2,
		data_in = DataIn2,
		data_out = DataOut2,
		msgs_in = MsgsIn2,
		msgs_out = MsgsOut2,
		auth_ack = AuthAck2,
		auth_nak = AuthNack2 }}};

handle_cast(sample, { T1, T2, Stats = #mesgd_stats{ samples = Samples,
	http_in = HttpIn, http_out = HttpOut, data_in = DataIn, data_out = DataOut, msgs_in = MsgsIn, msgs_out = MsgsOut,
	auth_ack = AuthAck, auth_nak = AuthNack }}) ->
		notify(Stats),
		{ noreply,  { T1, T2, Stats#mesgd_stats{ 
			http_in = window(HttpIn,Samples),
			http_out = window(HttpOut,Samples),
			data_in = window(DataIn,Samples),
			data_out = window(DataOut,Samples),
			msgs_in = window(MsgsIn,Samples),
			msgs_out = window(MsgsOut,Samples),
			auth_ack = window(AuthAck,Samples),
			auth_nak = window(AuthNack,Samples) }}};

handle_cast(store, { T1, T2, Stats = #mesgd_stats{  time = Time, samples = Samples, duration = Duration, domain = Domain,
	http_in = HttpIn, http_out = HttpOut, data_in = DataIn, data_out = DataOut, msgs_in = MsgsIn, msgs_out = MsgsOut,
	auth_ack = AuthAck, auth_nak = AuthNack }}) ->
	Summary = #mesgd_stats{
		time = Time,
		samples = Samples,
		duration = Duration,
		domain = Domain,
		http_in = persec(HttpIn,Duration),
		http_out = persec(HttpOut,Duration),
		data_in = persec(DataIn,Duration),
		data_out = persec(DataOut,Duration),
		msgs_in = persec(MsgsIn,Duration),
		msgs_out = persec(MsgsOut,Duration),
		auth_ack = persec(AuthAck,Duration),
		auth_nak = persec(AuthNack,Duration)
	},
	F = fun() ->
		mnesia:write(Summary)
	end,
	mnesia:activity(transaction,F),
	{ noreply, { T1, T2, Stats#mesgd_stats{ time = erlang:system_time() } } };

handle_cast(Message,State) ->
	error_logger:error_msg("Unknown message ~p~n", [ Message ]),
	{ noreply, State }.

handle_info(Message,State) ->
	error_logger:error_msg("Unknown message ~p~n", [ Message ]),
	{ noreply, State }.


terminate(_Reason, { T1, T2, _ }) ->
	timer:cancel(T1),
	timer:cancel(T2),
	ok.

code_change(_Vsn,_Extra,State) ->
	{ ok, State }.

%% return a list of samples with a 0 as the first term (reverse chrono)
window(List,Samples) ->
	Remainder = case length(List) > Samples of
		true ->
			lists:droplast(List);
		_ ->
			List
	end,
	[ 0 | Remainder ].
	
%% summarize the latest data point
latest(List) ->
	lists:nth(1,List).

persec(List,Duration) ->
	lists:sum(List) / Duration.	

update(Key,Stat = [N|T],Event) ->
	case proplists:get_value(Key,Event) of
		Value when is_integer(Value) -> 
			[ N + Value | T ];
		_ -> Stat
	end.

notify(#mesgd_stats{ master = Master, samples = Samples, duration = Duration, domain = Domain, 
	http_in = HttpIn, http_out = HttpOut, data_in = DataIn, data_out = DataOut, msgs_in = MsgsIn, msgs_out = MsgsOut,
	auth_ack = AuthAck, auth_nak = AuthNack }) ->
	Message = [
		{ time, erlang:system_time() },
		{ duration, Duration },
		{ samples, Samples },
		{ domain, Domain },
		{ http_in, latest(HttpIn) },
		{ http_out, latest(HttpOut) },
		{ data_in, latest(DataIn) },
		{ data_out, latest(DataOut) },
		{ msgs_in, latest(MsgsIn) },
		{ msgs_out, latest(MsgsOut) },
		{ auth_ack, latest(AuthAck) },
		{ auth_nak, latest(AuthNack) },
		{ http_in_ps, persec(HttpIn,Duration) },
		{ http_out_ps, persec(HttpOut,Duration) },
		{ data_in_ps, persec(DataIn,Duration) },
		{ data_out_ps, persec(DataOut,Duration) },
		{ msgs_in_ps, persec(MsgsIn,Duration) },
		{ msgs_out_ps, persec(MsgsOut,Duration) },
		{ auth_ack_ps, persec(AuthAck,Duration) },
		{ auth_nak_ps, persec(AuthNack,Duration) }
	],
	rpc:call(Master,mesgd_master,log,[Message]).

install(Nodes) ->
	{ atomic, ok } = mnesia:create_table(mesgd_stats, [
		{ attributes, record_info(fields,mesgd_stats) },
		{ disc_copies, Nodes }]).
