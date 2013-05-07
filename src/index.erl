-module(index).
-export([ new/0, add/2, add/3, tokenize/1, tokenize/4, index/5]).

new() ->
	ets:new(search_index, [set]).

add(Index, Filename) when is_list(Filename) ->
	{ ok, Bin } = file:read_file(Filename),
	add(Index,Filename,Bin).

add(Index, Filename, Bin ) when is_binary(Bin)  ->
	{ ok, Tokens } = tokenize(Bin),
	{ ok, Index2 } = index(Index,Filename,0,length(Tokens),Tokens),
	Index2.
	
tokenize(Bin) ->
	tokenize(0,[],nil, binary:bin_to_list(Bin)).

tokenize(_N, Tokens, nil, [] ) ->
	{ ok, Tokens };
tokenize(_N, Tokens, W, [] ) ->
	{ ok, [ W | Tokens ] };
tokenize(N, Tokens, W, [C|Letters]) ->
	case { W, lists:member(C," <>.,!?-()[]{}:\";~\n\t\"+-*/\\") } of
		{ nil, true } -> 
			tokenize(N+1, Tokens, nil, Letters);
		{ {Q,Cs}, true } ->
			tokenize(N+1, [{Q, list_to_atom(lists:reverse(Cs))}|Tokens], nil, Letters);
		{ nil, false } ->
			tokenize(N+1, Tokens, { N, [C] }, Letters);
		{ {Q,Cs}, false } ->
			tokenize(N+1, Tokens, { Q, [C|Cs] }, Letters)
	end.
			
index(Index,_Filename,_N, _L, []) ->
	{ ok, Index };

index(Index,Filename,N,L,[{Q,W}|Tokens]) ->
	case ets:lookup(Index,W) of
		[] ->
			ets:insert(Index,{W,[ { Filename, 1, 1/L,[Q] }]}),
			index(Index,Filename,N+1,L,Tokens);
		[{W, [{Filename, Count, _Frequency, Offsets }|Rest]}] -> 
			ets:insert(Index,{W, [{Filename, Count+1, (Count+1)/L, [Q|Offsets]}|Rest]}),
			index(Index,Filename,N+1,L,Tokens);
		[{W, Rest}] ->
			ets:insert(Index,{W, [{Filename, 1, 1/L, [Q]}|Rest]}),
			index(Index,Filename,N+1,L,Tokens)
	end.
