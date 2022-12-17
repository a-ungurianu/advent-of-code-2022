-module(map_utils).
-author("a-ungurianu").

-export([
    up/1,
    down/1,
    left/1,
    right/1,
    move/2,
    parse_into_map/1,
    map_to_string/3
]).


up({R,C}) -> {R - 1, C}.
down({R,C}) -> {R + 1, C}.
left({R,C}) -> {R, C - 1}.
right({R,C}) -> {R, C + 1}.

move(up, Pos) -> up(Pos);
move(down, Pos) -> down(Pos);
move(left, Pos) -> left(Pos);
move(right, Pos) -> right(Pos).

parse_row(Line, RowIdx) -> parse_row(Line, RowIdx, 0).

parse_row([], _RowIdx, _ColIdx) -> #{};
parse_row([Val| Rest], RowIdx, ColIdx) -> D = parse_row(Rest, RowIdx, ColIdx + 1), D#{ {RowIdx, ColIdx} => Val}.

parse_into_map(Lines) -> parse_into_map(Lines, 0).

parse_into_map([], _RowIdx) -> #{};
parse_into_map([Line | Rest], RowIdx) -> maps:merge(parse_into_map(Rest, RowIdx + 1), parse_row(Line, RowIdx)). 

get_pos_c(PointsMap, Pos) ->
    case PointsMap of
		#{Pos := V} -> V;
		#{} -> "."
	end.

map_to_string(PointsMap, _, BottomRight, BottomRight) -> get_pos_c(PointsMap, BottomRight);
map_to_string(PointsMap, {TL_X, _}=TopLeft, {BR_X, _}= BottomRight, {BR_X, P_Y}=Pos) -> 
    get_pos_c(PointsMap, Pos) ++ "\n" ++ map_to_string(PointsMap, TopLeft, BottomRight,{TL_X, P_Y + 1});
map_to_string(PointsMap, TopLeft, BottomRight, {X,Y} = Pos) ->
    get_pos_c(PointsMap, Pos) ++ map_to_string(PointsMap, TopLeft, BottomRight, {X + 1, Y}).
map_to_string(PointsMap, TopLeft, BottomRight) -> map_to_string(PointsMap, TopLeft, BottomRight, TopLeft).