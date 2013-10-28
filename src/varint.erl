-module(varint).
-export([ encode/1, decode/1, peek/2, poke/2, test/0 ]).


decode(<<>>) ->
	[];
decode(Bin) when is_binary(Bin) ->
	decode(Bin,[]).

decode(<<>>,List) when is_list(List) ->
	lists:reverse(List);
decode(Bin,List) when is_binary(Bin), is_list(List) ->
	{ Int, Bin2 } = decode(Bin,0,1),
	decode(Bin2, [Int|List]).

decode(<<>>, Y, _Z) ->	%% malencoded data!
	{ Y, <<>> };
decode(<<0:1,X:7,Bin/binary>>, Y, Z) ->
	{ Z * X + Y, Bin };
decode(<<1:1,X:7,Bin/binary>>, Y, Z) ->
	decode(Bin, Z * X + Y, 128*Z).



encode([]) ->
	<<>>;
encode(List) when is_list(List) ->
	encode(List,<<"">>).

encode([],Bin) when is_binary(Bin) ->
	Bin;
encode([H|T],Bin) when is_binary(Bin) ->
	Varint = encode(H div 128,H rem 128,<<"">>),
	encode(T,<<Bin/binary,Varint/binary>>).

encode(0,X,Bin) when is_binary(Bin), X < 128 ->
	<<Bin/binary,0:1,X:7>>;
encode(Y,X,Bin) when is_binary(Bin) ->
	encode(Y div 128, Y rem 128, <<Bin/binary,1:1,X:7>>).

peek(<<>>,_) -> { [], <<>> };		% no data
peek(_,0) -> { [], <<>> };		% bad count
peek(Bin,N) when is_binary(Bin) ->
	peek(Bin,N,[]).

peek(Bin,0,List) when is_binary(Bin), is_list(List) ->
	{ lists:reverse(List), Bin };	
peek(Bin,N,List) when is_binary(Bin), is_list(List) ->
	{ Int, Bin2 } = decode(Bin,0,1),
	peek(Bin2,N-1,[Int|List]).

poke(List,Bin) ->
	Head = encode(List),
	<<Head/binary,Bin/binary>>.

test() ->
	<<0:8>> = encode([0]),
	<<1:8>> = encode([1]),
	<<127:8>> = encode([127]),
	<<1:1,0:7,0:1,1:7>> = encode([128]),
	<<1:1,1:7,0:1,1:7>> = encode([129]),
	<<1:1,0:7,0:1,1:7,1:1,1:7,0:1,1:7>> = encode([128,129]),
	[0] = decode(<<0:8>>),
	[1] = decode(<<0:1,1:7>>),
	[1,1] = decode(<<0:1,1:7,0:1,1:7>>),
	[128] = decode(<<1:1,0:7,0:1,1:7>>),
	[129] = decode(<<1:1,1:7,0:1,1:7>>),
	[1,1,1] = decode(<<0:1,1:7,0:1,1:7,0:1,1:7>>),
	[2,128,0] = decode(<<0:1,2:7,1:1,0:7,0:1,1:7,0:8>>),
	decode(encode([10101,23456,123,1,0,0,675432])),
	{ [101,300,202], <<1:8,1:8,1:8>> } = peek(encode([101,300,202,1,1,1]), 3),
	Str = encode([101,300,202,1,1,1]),
	Str = poke([101,300,202], <<1,1,1>>).
	


