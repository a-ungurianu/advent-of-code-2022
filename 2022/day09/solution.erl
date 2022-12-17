-module(solution).
-author("a-ungurianu").

%% API
-export([
	first/1,
	second/1
]).

rope_to_point_map(Rope) -> rope_to_point_map(Rope, 0).
rope_to_point_map([], _) -> #{};
rope_to_point_map([Tip|Rest], Idx) -> 
	Res = rope_to_point_map(Rest, Idx + 1),
	Res#{
		Tip => integer_to_list(Idx)
	}.

print_rope(Rope, Size)->
	map_utils:map_to_string(rope_to_point_map(Rope), {-Size, -Size},{Size, Size}).

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

adjust_rope_segment({R1, C1}=Pos, {R2, C2}=InFront) ->
	CDist = max(abs(R1-R2), abs(C1-C2)),
	if
		CDist =< 1 -> Pos;
		true -> adjust_rope_segment(Pos, InFront, true)
	end.
adjust_rope_segment({R, PC}=Pos, {R,FC}, true) -> 
	if
		PC > FC -> map_utils:move(left, Pos);
		PC < FC -> map_utils:move(right, Pos)
	end;
adjust_rope_segment({PR, C}=Pos, {FR,C}, true) -> 
	if
		PR > FR -> map_utils:move(up, Pos);
		PR < FR -> map_utils:move(down, Pos)
	end;
adjust_rope_segment({PR, PC}=Pos, {FR,FC}, true) -> 
	if
		PR > FR, PC > FC -> map_utils:move(left, map_utils:move(up, Pos));
		PR > FR, PC < FC -> map_utils:move(right, map_utils:move(up, Pos));
		PR < FR, PC > FC -> map_utils:move(left, map_utils:move(down, Pos));
		PR < FR, PC < FC -> map_utils:move(right, map_utils:move(down, Pos))
	end.
	
adjust_rope([Last]) -> [Last];
adjust_rope([Lead, Follow | Rest]) ->
	NewFollow = adjust_rope_segment(Follow, Lead),
	[Lead | adjust_rope([NewFollow | Rest])].


apply_move(Dir, Rope, TailTouched) ->
	[Head | Rest] = Rope,
	MovedHead = map_utils:move(Dir, Head),
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
