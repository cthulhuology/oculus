-module(index_server).
-behavior(gen_server).
-vsn("1").
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).
-export([ start/2, test/0, search/2, index/2 ]).
-record(index, { id, user, docs, terms, filters, topics }).

-define(SELF(U,I), list_to_atom("index_server_" ++ U ++ "_" ++ I)).


%% Public API

start(UserId,IndexId) ->
	{ok, Pid } = gen_server:start_link(?MODULE,[UserId,IndexId],[]),
	true = register(?SELF(UserId,IndexId),Pid),
	?SELF(UserId,IndexId).

search(Index,Term) ->
	gen_server:call(Index, { search, Term }).

index(Index,Url) ->
	gen_server:call(Index, { index, Url }).

test() ->
	%% Before you run this test run:
	%%
	%% 	ibrowse:start().
	%
	%% and make sure that the searchapi is running
	%%
	%% 	mix server
	%%
	%% Or it will fail
	%%
	Index = start("testuser","testindex"),
	[] = search(Index,"test"),
	{ 1, 27 } = index(Index, "http://dloh.org:4000/test"),
	[ "http://dloh.org:4000/test" ] = search(Index,"test"),
	[] = search(Index,"foobar"),
	ok.

%% gen_server behavior
init([UserId,IndexId]) ->
	case filelib:is_file(UserId ++ "/" ++ IndexId ++ ".zip") of
		false  -> archive:new(UserId,IndexId);
		true -> ok
	end,
	archive:open(UserId,IndexId),
	Path = UserId ++ "/" ++ IndexId ++ "/",
	{ok,D} = document:load(Path ++ "docs.tbl"),
	{ok,T} = index:load(Path ++ "terms.tbl"),
	{ ok, #index{ id = IndexId, user = UserId, docs = D, terms = T } }.

handle_call({ index, Url }, _From, #index{ docs = D, terms = T } = State) ->
	Doc = document:add(D,Url),
	Body = document:fetch(Url),	
	Count = document:update(D, index:add(T,Doc,Body)),
	{ reply, { Doc, Count }, State };
handle_call({ search, Term }, _From, #index{ docs = D, terms = T } = State) ->
	{ reply, document:search(D,T,Term), State };	
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



