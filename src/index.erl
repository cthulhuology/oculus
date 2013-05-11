-module(index).
-export([ new/0, get/2, add/2, add/3, tokenize/1, tokenize/4, index/5]).


new() ->
	ets:new(search_index, [set]).

get(Index, Url) when is_list(Url) ->
	case ibrowse:send_req(Url,[],get) of
		{ok, "200", _Headers, Body } -> add(Index, Url, Body);
		{ok, _, _Headers, _Body } -> true
	end.

add(Index, Filename) when is_list(Filename) ->
	{ ok, Bin } = file:read_file(Filename),
	add(Index,Filename,Bin).

add(Index, Filename, Bin ) when is_binary(Bin)  ->
	add(Index,Filename,binary:bin_to_list(Bin));

add(Index, Filename, List ) when is_list(List) ->
	{ ok, Tokens } = tokenize(List),
	{ ok, Index2 } = index(Index,Filename,0,length(Tokens),Tokens),
	Index2.
	
tokenize(List) ->
	tokenize(0,[],nil, List).

tokenize(_N, Tokens, nil, [] ) ->
	{ ok, Tokens };
tokenize(_N, Tokens, W, [] ) ->
	{ ok, [ W | Tokens ] };
tokenize(N, Tokens, W, [C|Letters]) ->
	case { W, lists:member(C," <>.,!?-()[]{}:\";~\n\t\"+-*/\\") } of
		{ nil, true } -> 
			tokenize(N+1, Tokens, nil, Letters);
		{ {Q,Cs}, true } ->
			tokenize(N+1, [{Q, lists:reverse(Cs)}|Tokens], nil, Letters);
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
			ets:insert(Index,{W, 1, [ { Filename, 1, 1/L,[Q] }]}),
			index(Index,Filename,N+1,L,Tokens);
		[{W, T, [{Filename, Count, _Frequency, Offsets }|Rest]}] -> 
			ets:insert(Index,{W, T+1, [{Filename, Count+1, (Count+1)/L, [Q|Offsets]}|Rest]}),
			index(Index,Filename,N+1,L,Tokens);
		[{W, T, Rest}] ->
			ets:insert(Index,{W, T+1, [{Filename, 1, 1/L, [Q]}|Rest]}),
			index(Index,Filename,N+1,L,Tokens)
	end.
