-module(solution).
-author("a-ungurianu").

%% API
-export([
	first/1,
	second/1
]).

parse_row(Line, RowIdx) -> parse_row(Line, RowIdx, 0).

parse_row([], _RowIdx, _ColIdx) -> [];
parse_row([Val| Rest], RowIdx, ColIdx) -> [{{RowIdx, ColIdx}, Val - $0} | parse_row(Rest, RowIdx, ColIdx + 1)].

parse(Input) -> parse(Input, 0).

parse([], _RowIdx) -> [];
parse([Line | Rest], RowIdx) -> parse(Rest, RowIdx + 1) ++ parse_row(Line, RowIdx).

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
	Rows = rows(Trees), Cols = cols(Trees),
	RowVisible = sets:union([sets:from_list(find_visible(Row)) || Row <- Rows]),
	RowReversedVisible = sets:union([sets:from_list(find_visible(lists:reverse(Row))) || Row <- Rows]),
	ColVisible = sets:union([sets:from_list(find_visible(Col)) || Col <- Cols]),
	ColReversedVisible = sets:union([sets:from_list(find_visible(lists:reverse(Col))) || Col <- Cols]),
	VisibleTrees = sets:union([RowVisible, RowReversedVisible, ColVisible, ColReversedVisible]),
	sets:size(VisibleTrees).

second(Input) ->
	0.
