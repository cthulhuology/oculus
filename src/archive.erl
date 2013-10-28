-module(archive).
-export([ new/2, open/2, close/2, test/0 ]).

% This module extracts the bits of the index as a set of ets files
%

new(UserId,IndexId) ->
	Path = UserId ++ "/" ++ IndexId ++ "/",
	ok = case filelib:is_dir(Path) of 
		false -> filelib:ensure_dir(Path);
		_ -> ok
	end,
	D = document:new(),
	ok = document:save(D,Path ++ "docs.tbl"),
	J = index:new(),
	ok = index:save(J,Path ++ "terms.tbl"),
	close(UserId,IndexId),
	ok.
	
open(UserId,IndexId) ->
	filelib:ensure_dir(UserId ++ "/" ++ IndexId ++ "/"),
	zip:unzip(UserId ++ "/" ++ IndexId ++ ".zip"),
	ok.

close(UserId,IndexId) ->
	Files = filelib:wildcard(UserId ++ "/" ++ IndexId ++ "/*.tbl"),
	zip:zip(UserId ++ "/" ++ IndexId ++ ".zip", Files ),
	[ file:delete(F) || F <- Files ],
	ok.
	
	
test() ->
	ok = new("testuser","testindex"),
	true = filelib:is_file("testuser/testindex.zip"),
	true = filelib:is_dir("testuser/testindex/"),
	false = filelib:is_file("testuser/testindex/docs.tbl"),
	ok = open("testuser","testindex"),
	true = filelib:is_file("testuser/testindex/docs.tbl"),
	true = filelib:is_file("testuser/testindex/terms.tbl"),
	ok = close("testuser","testindex"),
	true = filelib:is_file("testuser/testindex.zip"),
	false = filelib:is_file("testuser/testindex/docs.tbl"),
	ok.	
