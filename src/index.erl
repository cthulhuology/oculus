-module(index).
-export([ new/0, add/3, load/1, save/2, tokenize/1, tokenize/4, index/5, search/2, test/0]).


new() ->
	ets:new(search_index, [set]).

load(Filename) ->
	ets:file2tab(Filename).

save(Index,Filename) ->
	ets:tab2file(Index,Filename).

strip_positions([],Acc) -> Acc;
strip_positions([ Doc, Count, Length | Rest ], Acc) ->
	strip_positions(lists:nthtail(Count,Rest), [ { Count / Length, Doc } | Acc ]).

ranksort(Bin) ->
	[ _Count | Docs ] = varint:decode(Bin),
	DocFreq = strip_positions(Docs,[]),
	RankedDocs = lists:reverse(lists:sort(fun({A,_DocA},{B, _DocB }) ->  A =< B end, DocFreq)),
	[ Doc ||  { _F, Doc } <- RankedDocs ].

search(Index, Term) when is_binary(Term) ->
	search(Index,binary:bin_to_list(Term));
search(Index, Term) when is_list(Term) ->
	case ets:lookup(Index,Term) of
	[] -> []; 
	[{Term, Bin }] -> ranksort(Bin)
	end.

add(Index, Doc, List ) when is_list(List) -> 
	{ ok, Text } = html:strip(List),
	{ ok, Tokens } = tokenize(Text),
	{ ok, Index, Doc, Count } = index(Index,Doc,0,length(Tokens),Tokens),
	{ Doc, Count }.
	
tokenize(List) ->
	tokenize(0,[],nil, List).

tokenize(_N, Tokens, nil, [] ) ->
	{ ok, Tokens };
tokenize(_N, Tokens, W, [] ) ->
	{ ok, [ W | Tokens ] };
tokenize(N, Tokens, W, [C|Letters]) ->
	case { W, lists:member(C," <>.,!?-()[]{}:\";~\n\t\"+-*/\\") } of
	{ nil, true } -> 
		tokenize(N, Tokens, nil, Letters);
	{ {Q,Cs}, true } ->
		tokenize(N+1, [{Q, lists:reverse(Cs)}|Tokens], nil, Letters);
	{ nil, false } ->
		tokenize(N, Tokens, { N, [C] }, Letters);
	{ {Q,Cs}, false } ->
		tokenize(N, Tokens, { Q, [C|Cs] }, Letters)
	end.
			
index(Index, Doc, N, _L, []) ->
	{ ok, Index, Doc, N};

index(Index,Doc,N,L,[{Q,W}|Tokens]) ->
	case ets:lookup(Index,W) of
	[] ->
		ets:insert(Index,{W,varint:encode([1, Doc, 1, L, Q ])}),
		index(Index,Doc,N+1,L,Tokens);
	[{W, Bin}] ->
		case varint:peek(Bin,4) of
		{[ T, Doc, Count, L ], Rest} -> 
			ets:insert(Index,{W,varint:poke([T+1, Doc, Count+1, L, Q], Rest)}),
			index(Index,Doc,N+1,L,Tokens);
		{[ T, X, Y, Z ], Rest} ->
			ets:insert(Index,{W,varint:poke([T+1, Doc, 1, L, Q, X, Y, Z], Rest)}),
			index(Index,Doc,N+1,L,Tokens)
		end
	end.


test() ->
	Index = new(),
	Doc123 = 123,
	Text123 = "This is a test of the emergency broadcast system; this is only a test.",
	add(Index,Doc123,Text123),
	Doc456 = 456,
	Text456 = "I am totally going to fail this test!",
	add(Index,Doc456,Text456),
	[] = search(Index,"foobar"),
	[ 123, 456 ] = search(Index,"test"),
	[ 456 ] = search(Index,"totally"),
	[ 456 ] = search(Index,"I"),
	Doc789 = 789,
	Text789 = "I like traffic lights, I like traffic lights,...",
	add(Index,Doc789,Text789),
	[ 789, 456 ] = search(Index,"I"),
	[ 789, 456 ] = search(Index,<<"I">>),
	{ok, Index}.
	
