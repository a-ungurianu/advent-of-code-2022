-module(solution).
-author("a-ungurianu").

%% API
-export([
	first/1,
	second/1
]).

find_by_value(Map, Value) ->
	maps:keys(maps:filter(fun (_, V) -> V == Value end, Map)).

char_to_height($S) -> 0;
char_to_height($E) -> $z - $a;
char_to_height(C) -> C - $a.

parse(Input) -> 
	Map = map_utils:parse_into_map(Input),
	Start = hd(find_by_value(Map, $S)),
	End = hd(find_by_value(Map, $E)),
	HeightMap = maps:map(fun (_, V) -> char_to_height(V) end, Map),
	{HeightMap, Start, End}.


is_possible_pos(Pos, CurrentHeight, Map, Visited) ->
	case Map of 
		#{Pos := Height} -> (Height =< CurrentHeight + 1) and (not sets:is_element(Pos, Visited));
		#{} -> false
	end.

find_path(Start, End, Map) -> find_path(End, Map, sets:from_list([Start]), [{Start, 0}]).
find_path(End, _Map, _Visited, [{End, Length} | _Rest]) -> Length;
find_path(_End, _Map, _Visited, []) -> deadend;
find_path(End, Map, Visited, [{Pos, Length}| Rest]) ->
	#{Pos := CurrentHeight} = Map,
	Moves = [fun map_utils:up/1, fun map_utils:down/1, fun map_utils:left/1, fun map_utils:right/1],
	PossibleMoves = lists:filter(fun (PPos) -> is_possible_pos(PPos, CurrentHeight, Map, Visited) end, lists:map(fun (Move) -> Move(Pos) end, Moves)),
	ToVisit = Rest ++ lists:map(fun (PPos) -> {PPos,Length + 1} end, PossibleMoves),
	NVisited = lists:foldl(fun(El, Acc) -> sets:add_element(El, Acc) end, Visited, PossibleMoves),
	find_path(End, Map, NVisited, ToVisit).

first(Input) ->
	{Map, Start, End} = parse(Input),
	find_path(Start, End, Map).

second(Input) ->
	{Map, _, End} = parse(Input),
	PossibleStarts = find_by_value(Map, 0),
	PathLengths = [ find_path(Start, End, Map) || Start <- PossibleStarts],
	lists:min(PathLengths).
