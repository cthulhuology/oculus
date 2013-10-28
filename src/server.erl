-module(server).
-behavior(gen_server).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

init([]) ->
	{ ok, [] }.

handle_call(_Msg, _From, State ) ->
	{ reply, ok, State }.

handle_cast(_Msg, State) ->
	{ noreply, State }.

handle_info(_Msg, State ) ->
	{ noreply, State }.


terminate(_Reason, State ) ->
	{ stop, State }.

code_change(_OldVsn, State, _Extra ) ->
	{ ok, State }.



