-module(solution).
-author("a-ungurianu").

%% API
-export([
	first/1,
	second/1
]).

parse(Input) -> maps:map(fun (_, V) -> V - $0 end, map_utils:parse_into_map(Input)).

rows(Trees) -> rows(Trees, 0).
rows(Trees, RowIdx) -> 
	ThisRow = lists:sort(fun({{_,C1}, _}, {{_,C2}, _}) -> C1 < C2 end, lists:filter(fun({{R,_}, _}) -> R == RowIdx end, Trees)),
	if
		length(ThisRow) > 0 -> [ThisRow | rows(Trees, RowIdx + 1)];
		true -> []
	end.

cols(Trees) -> cols(Trees, 0).
cols(Trees, ColIdx) -> 
	ThisRow = lists:sort(fun({{R1,_}, _}, {{R2,_}, _}) -> R1 < R2 end, lists:filter(fun({{_, C}, _}) -> C == ColIdx end, Trees)),
	if
		length(ThisRow) > 0 -> [ThisRow | cols(Trees, ColIdx + 1)];
		true -> []
	end.

find_visible(Trees) -> find_visible(Trees, -1).
find_visible([], _) -> [];
find_visible([{Coord, Height}| Rest], LastHeight) -> 
	if 
		Height > LastHeight -> [Coord | find_visible(Rest, Height)];
		true -> find_visible(Rest, LastHeight)
	end.



first(Input) ->
	Trees = parse(Input),
	Rows = rows(maps:to_list(Trees)), Cols = cols(maps:to_list(Trees)),
	RowVisible = sets:union([sets:from_list(find_visible(Row)) || Row <- Rows]),
	RowReversedVisible = sets:union([sets:from_list(find_visible(lists:reverse(Row))) || Row <- Rows]),
	ColVisible = sets:union([sets:from_list(find_visible(Col)) || Col <- Cols]),
	ColReversedVisible = sets:union([sets:from_list(find_visible(lists:reverse(Col))) || Col <- Cols]),
	VisibleTrees = sets:union([RowVisible, RowReversedVisible, ColVisible, ColReversedVisible]),
	sets:size(VisibleTrees).

score_dir(Pos, Height, Trees, Move) -> 
	case Trees of 
		#{Pos := H} -> if 
				H < Height -> score_dir(Move(Pos), Height, Trees, Move) + 1;
				true -> 1
			end;
		#{} -> 0
	end. 

score(Pos, Height, Trees) ->
	Moves = [fun map_utils:up/1, fun map_utils:down/1, fun map_utils:left/1, fun map_utils:right/1],
	Scores = lists:map(fun (Dir) -> score_dir(Dir(Pos), Height, Trees, Dir) end, Moves),
	lists:foldl(fun (S, Acc) -> Acc * S end, 1, Scores).


second(Input) ->
	Trees = parse(Input),
	lists:max(maps:values(maps:map(fun (Pos, Height) -> score(Pos, Height, Trees) end, Trees))).
