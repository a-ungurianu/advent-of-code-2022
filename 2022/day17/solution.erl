-module(solution).
-author("a-ungurianu").

%% API
-export([
	first/1,
	second/1
]).

% y
% 0####
%  0123x
-define(DASH_PIECE, [{0,0}, {1,0}, {2,0}, {3,0}]).

% y
% 2.#.
% 1###
% 0.#.
%  012x
-define(CROSS_PIECE, [{1,0}, {0,1}, {1,1}, {2,1}, {1,2}]).

% y
% 2..#
% 1..#
% 0###
%  012x
-define(L_PIECE, [{0,0}, {1,0}, {2,0}, {2,1}, {2,2}]).

% y
% 3#
% 2#
% 1#
% 0#
%  0x
-define(TALL_PIECE, [{0,0}, {0,1}, {0,2}, {0,3}]).

% y
% 1##
% 0##
%  01x
-define(SQUARE_PIECE, [{0,0}, {1,0}, {0,1}, {1,1}]).

cycler(Things) -> {Things, Things}.

next_cycle({[Current], Things}) -> {Current, {Things, Things}};
next_cycle({[Current| Rest], Things}) -> {Current, {Rest,Things}}.

parse_jets([]) -> [];
parse_jets([JetS | Rest]) ->
	Jet = case JetS of
		$> -> right;
		$< -> left
	end,
	[Jet | parse_jets(Rest)].

parse(Input) ->
	parse_jets(hd(Input)).


add2({X1, Y1}, {X2, Y2}) -> {X1 + X2, Y1 + Y2}.

is_available({X, _}, _) when X < 0 ; X >= 7 -> false;
is_available({_, Y}, _) when Y =< 0 -> false;
is_available(Pos, Elements) -> 
	not sets:is_element(Pos, Elements).

move_piece(Piece, Offset) -> lists:map(fun (P) -> add2(P, Offset) end, Piece).

try_move(Piece, Elements, OffSet) -> 
	NewPossiblePiece = move_piece(Piece, OffSet),
	case lists:all(fun (Pos) -> is_available(Pos, Elements) end, NewPossiblePiece) of
		true -> {moved, NewPossiblePiece};
		false -> {nope, Piece}
	end.

jet_to_offset(left) -> {-1, 0};
jet_to_offset(right) -> {1, 0}.


step_rock(CurrentPiecePos, JetsCycler, Elements) -> 
	{Jet, NextJetsCycler} = next_cycle(JetsCycler),
	{_, JettedPiecePos} = try_move(CurrentPiecePos, Elements, jet_to_offset(Jet)),
	case try_move(JettedPiecePos, Elements, {0, -1}) of
		{nope, LastMovePiece}  -> {NextJetsCycler, LastMovePiece};
		{moved, LastMovePiece} -> step_rock(LastMovePiece, NextJetsCycler, Elements)
	end.

find_max_y([]) -> -1;
find_max_y([{_, Y} | Rest]) -> max(Y, find_max_y(Rest)).

board_to_point_map(Elements, Piece) ->
	maps:merge(maps:from_keys(lists:map(fun ({X,Y}) -> {X, -Y} end, sets:to_list(Elements)), "#"), maps:from_keys(lists:map(fun ({X,Y}) -> {X, -Y} end, Piece), "@")).

board_to_string(Elements, Piece, MaxHeight) ->
	map_utils:map_to_string(board_to_point_map(Elements, Piece), {0,-MaxHeight-1}, {6,-MaxHeight + 40}).

drop_rock(Piece, JetsCycler, Elements, MaxHeight) ->
	OffSet = {2, MaxHeight + 4},
	PlacedPiece = move_piece(Piece, OffSet),
	{NextJetsCycler, FinalPiecePos} = step_rock(PlacedPiece, JetsCycler, Elements),
	{NextJetsCycler, sets:union(sets:from_list(FinalPiecePos), Elements), find_max_y(FinalPiecePos)}.

drop_rocks(_, _, 0, Elements, MaxHeight, _) -> {Elements, MaxHeight};
drop_rocks({P, _}=PieceCycler, {J,_}=JetsCycler, Count, Elements, MaxHeight, PrevConfigurations) ->
	CurrentBoardView = board_to_string(Elements, [], MaxHeight),
	CycleKey = {length(P), length(J), CurrentBoardView},
	case PrevConfigurations of
		#{CycleKey := {CachedCount, CachedMaxHeight}} -> 
			CycleSize = CachedCount - Count,
			CycleHeightAdd = MaxHeight - CachedMaxHeight,
			CyclesRemaining = Count div CycleSize,
			RemainingFromCycle = Count rem CycleSize,
			io:format("Found match @ ~w <- ~w : cycle size=~w, height_diff=~w, cyclesRemaining=~w\n", [Count, CachedCount, CycleSize, CycleHeightAdd, CyclesRemaining]),
			drop_rocks(PieceCycler, JetsCycler, RemainingFromCycle, sets:from_list(move_piece(sets:to_list(Elements), {0, CycleHeightAdd * CyclesRemaining})), MaxHeight + CycleHeightAdd * CyclesRemaining, #{});
		#{} -> 
			{Piece, NextPieceCycler} = next_cycle(PieceCycler),
			{NextJetsCycler, NewElements, NewMaxHeight} = drop_rock(Piece, JetsCycler, Elements, MaxHeight),
			drop_rocks(NextPieceCycler, NextJetsCycler, Count - 1, NewElements, max(NewMaxHeight, MaxHeight), PrevConfigurations#{CycleKey => {Count, MaxHeight}})
	end.

drop_rocks(Jets, Count) -> 
	PieceCycler = cycler([?DASH_PIECE, ?CROSS_PIECE, ?L_PIECE, ?TALL_PIECE, ?SQUARE_PIECE]),
	JetsCycler = cycler(Jets),
	drop_rocks(PieceCycler, JetsCycler, Count, sets:new(), 0, #{}).

first(Input) ->
	Jets = parse(Input),
	{_, MaxHeight} = drop_rocks(Jets, 2022),
	MaxHeight.

second(Input) ->
	Jets = parse(Input),
	{_, MaxHeight} = drop_rocks(Jets, 1000000000000),
	MaxHeight.
