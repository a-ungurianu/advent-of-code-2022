-module(solution).
-author("a-ungurianu").

%% API
-export([
	first/1,
	second/1
]).

points_to_map(PointsMap, Size, Size, Size) -> string:pad(integer_to_list(Size), 2, leading) ++ "\n";
points_to_map(PointsMap, Size, Row, Size) -> string:pad(integer_to_list(Row), 2, leading) ++ "\n" ++ points_to_map(PointsMap, Size, Row + 1, -Size);
points_to_map(PointsMap, Size, Row, Col) ->
	case PointsMap of
		#{{Row, Col} := V} -> V;
		#{} -> "."
	end ++ points_to_map(PointsMap, Size, Row, Col + 1).
points_to_map(PointsMap, Size) -> points_to_map(PointsMap, Size, -Size, -Size).


rope_to_point_map(Rope) -> rope_to_point_map(Rope, 0).
rope_to_point_map([], _) -> #{};
rope_to_point_map([Tip|Rest], Idx) -> 
	Res = rope_to_point_map(Rest, Idx + 1),
	Res#{
		Tip => integer_to_list(Idx)
	}.

print_rope(Rope, Size)->
	points_to_map(rope_to_point_map(Rope), Size).

parse_move(Line) -> 
	[DirS, AmountS] = string:split(Line, " "),
	Move = case DirS of
		"R" -> right;
		"L" -> left;
		"U" -> up;
		"D" -> down
	end,
	{Move, list_to_integer(AmountS)}.

parse(Input) -> lists:map(fun parse_move/1, Input).

move({R,C}, up) -> {R-1, C};
move({R,C}, down) -> {R+1, C};
move({R,C}, left) -> {R, C-1};
move({R,C}, right) -> {R, C+1}.

adjust_rope_segment({R1, C1}=Pos, {R2, C2}=InFront) ->
	CDist = max(abs(R1-R2), abs(C1-C2)),
	if
		CDist =< 1 -> Pos;
		true -> adjust_rope_segment(Pos, InFront, true)
	end.
adjust_rope_segment({R, PC}=Pos, {R,FC}, true) -> 
	if
		PC > FC -> move(Pos, left);
		PC < FC -> move(Pos, right)
	end;
adjust_rope_segment({PR, C}=Pos, {FR,C}, true) -> 
	if
		PR > FR -> move(Pos, up);
		PR < FR -> move(Pos, down)
	end;
adjust_rope_segment({PR, PC}=Pos, {FR,FC}, true) -> 
	if
		PR > FR, PC > FC -> move(move(Pos, up), left);
		PR > FR, PC < FC -> move(move(Pos, up), right);
		PR < FR, PC > FC -> move(move(Pos, down), left);
		PR < FR, PC < FC -> move(move(Pos, down), right)
	end.
	
adjust_rope([Last]) -> [Last];
adjust_rope([Lead, Follow | Rest]) ->
	NewFollow = adjust_rope_segment(Follow, Lead),
	[Lead | adjust_rope([NewFollow | Rest])].


apply_move(Dir, Rope, TailTouched) ->
	[Head | Rest] = Rope,
	MovedHead = move(Head, Dir),
	NewRope = adjust_rope([MovedHead| Rest]),
	NTailTouched = sets:add_element(lists:last(NewRope), TailTouched),
	{NewRope, NTailTouched}.

apply_move(_Dir, Rope, TailTouched, 0) -> {Rope, TailTouched};
apply_move(Dir, Rope, TailTouched, Count) ->
	{NewRope, NewTailTouched} = apply_move(Dir, Rope, TailTouched),
	apply_move(Dir, NewRope, NewTailTouched, Count - 1).

apply_moves(Rope, Moves) -> apply_moves(Rope, Moves, sets:from_list([lists:last(Rope)])).
apply_moves(Rope, [], TailTouched) -> {Rope, TailTouched};
apply_moves(Rope, [{Dir, Count}| Moves], TailTouched) ->
	{NewRope, NewTailTouched} = apply_move(Dir, Rope, TailTouched, Count),
	apply_moves(NewRope, Moves, NewTailTouched).

first(Input) ->
	Moves = parse(Input),
	Rope = [{0,0}, {0,0}],
	{_FinalRope, TailTouched} = apply_moves(Rope, Moves),
	sets:size(TailTouched).

second(Input) ->
	Moves = parse(Input),
	Rope = [{0,0}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0}],
	{_FinalRope, TailTouched} = apply_moves(Rope, Moves),
	sets:size(TailTouched).
