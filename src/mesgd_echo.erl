-module(mesgd_echo).

-export([ echo/3 ]).

echo(Pid,Path,connected) ->
	error_logger:info_msg("From ~p at ~p connected", [ Pid, Path ]);
echo(Pid,Path,closed) ->
	error_logger:info_msg("From ~p at ~p closed", [ Pid, Path ]);
echo(Pid,Path,Data) ->
	error_logger:info_msg("From ~p at ~p got ~p", [ Pid, Path, Data ]),
	Pid ! Data.

