-module(document).
-export([ new/0, load/1, save/2, add/2, fetch/1, update/2, find/2, count/2, search/3, test/1]).

new() ->
	D = ets:new(document_index, [set]),
	ets:insert(D,{max,1}),
	D.

load(Filename) ->
	ets:file2tab(Filename).

save(D,Filename) ->
	ets:tab2file(D,Filename).

search(D,I,Term) ->
	case index:search(I,Term) of
		[] -> [];
		Docs -> [ find(D,Id) || Id <- Docs ]
	end.

add(D,Url) when is_list(Url) ->
	[ {max, Max }] = ets:lookup(D,max),
	ets:insert(D, {max, Max+1}),
	ets:insert(D, { Max, Url, 0 }), % Url, Id, Count 
	Max.	

fetch(Url) when is_list(Url) ->
	case ibrowse:send_req(Url,[],get) of
	{ok, "200", _Headers, Body } -> 
		Body;
	{ok, _, _Headers, _Body } -> 
		""
	end.

update(D, {Id,Count}) ->
	[{ Id, Url, _OldCount }] = ets:lookup(D,Id),
	ets:insert(D,{Id, Url, Count}),
	Count.

find(D,Id) ->
	[{ Id, Url, _Count }] = ets:lookup(D,Id),
	Url.

count(D,Id) ->
	[{ Id, _Url, Count }] = ets:lookup(D,Id),
	Count.
	
test(Filename) ->
	{ok, Data} = file:read_file(Filename),
	Urls = string:tokens(binary:bin_to_list(Data),"\n"),
	D = new(),
	J = index:new(),
	[ update(D,index:add(J, add(D,U), fetch(U))) || U <- Urls ],
	save(D,"doc.idx"),
	index:save(J,"index.idx"),	
	{ D, J }.
