-module(html).
-export([ strip/1, strip/3, test/0, strip_script/3]).

%% Public Interface takes a bin or a list
strip(Bin) when is_binary(Bin)  ->
	strip([],false,binary:bin_to_list(Bin));
strip(List) when is_list(List) ->
	strip([],false,List).

strip_script(Words,_Tag, "/script" ++ Letters) ->
	strip(Words,false,Letters);
strip_script(Words,_Tag, "script" ++ Letters) ->
	strip_script_words(Words,Letters);
strip_script(Words,Tag,Letters) ->
	strip(Words,Tag,Letters).

strip_script_words(Words,[$<|Letters]) ->
	strip_script(Words,true,Letters);
strip_script_words(Words,[_|Letters]) ->
	strip_script_words(Words,Letters).

%% Strips out all html tags and newline tabs
strip(Words,_Tag, []) ->
	{ ok, lists:reverse(Words) };
strip(Words, Tag, [C|Letters]) ->
	case {Tag,C} of
	{_,$<} -> strip_script(Words, true, Letters);
	{_,$>} -> strip([32| Words], false, Letters);
	{true,_} -> strip(Words, Tag, Letters);
	{false,10} -> strip([32|Words], Tag, Letters);
	{false,9} -> strip([32|Words], Tag, Letters);
	{false,_} -> strip([C|Words], Tag, Letters)
	end.


%% Test common cases
test() ->
	ibrowse:start(),
	{ok, _Status, _Headers, Body } = ibrowse:send_req("http://dloh.org/A-Whole-New-Style",[],get),
	strip(Body).

