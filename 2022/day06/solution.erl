-module(solution).
-author("a-ungurianu").

%% API
-export([
	first/1,
	second/1
]).

are_all_different(Xs) ->
	length(sets:to_list(sets:from_list(Xs))) == length(Xs).

find_marker(Line, MarkerSize) -> find_marker(Line, MarkerSize, 0).
find_marker(Line, MarkerSize, Pos) -> 
	CandidateMarker = lists:sublist(Line, MarkerSize),
	CandidateMarkerDiff = are_all_different(CandidateMarker),
	if 
		CandidateMarkerDiff -> Pos + MarkerSize;
		true -> find_marker(tl(Line), MarkerSize, Pos + 1)
	end.

first(Input) ->
	find_marker(hd(Input), 4).

second(Input) ->
	find_marker(hd(Input), 14).
