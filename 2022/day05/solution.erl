-module(solution).
-author("a-ungurianu").

%% API
-export([
	first/1,
	second/1
]).

stack_end(Line) -> 
	case Line of
		[32 , 49 | _Rest] -> true;
		Line -> false
	end.

get_stack_section(Input) ->
	Stacks = lists:takewhile(fun (Line) -> not stack_end(Line) end, Input),
	Rest = lists:nthtail(length(Stacks) + 2, Input), % + 2 to skip the numbers and new line
	{Stacks, Rest}.

skip_one(List) ->
	case List of
		[_| Rest] -> Rest;
		List -> List
	end.

parse_crate(Line) -> 
	case Line of
		[91,C,93|Rest] -> {C, skip_one(Rest)}; %nthtail to skip the space
		"   " ++ Rest -> {missing, skip_one(Rest)}
	end. 

parse_stack_row(Row) -> parse_stack_row(Row, []).

parse_stack_row(Row, Res) -> 
	case parse_crate(Row) of
		{C, []} -> [C |Res];
		{C, Rest} -> parse_stack_row(Rest, [C|Res])
	end.

transpose([[]|_]) -> [];
transpose(M) ->
  [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].

trim_missing([missing| Rest]) -> trim_missing(Rest);
trim_missing(L) -> L. 

parse_stacks(Stacks) -> 
	Tokenized = lists:map(fun parse_stack_row/1, Stacks),
	lists:reverse(lists:map(fun trim_missing/1, transpose(Tokenized))).

parse_move(Line) ->
	Regex = "move (\\d+) from (\\d+) to (\\d+)",
	case re:run(Line, Regex, [{capture, all_but_first, binary}]) of
		{match, [Count, From, To]} -> {binary_to_integer(Count), binary_to_integer(From) - 1, binary_to_integer(To) - 1}
	end.


parse(Input) -> {Stacks, Rest} = get_stack_section(Input), {parse_stacks(Stacks), lists:map(fun parse_move/1, Rest)}.

remove_from_stack(Stacks, From, Count) ->
	[CurStack | Rest] = Stacks,
	if
		From == 0 -> {lists:sublist( CurStack, Count), [lists:nthtail(Count, CurStack)|Rest]};
		From > 0 -> {Removed, RestRemoved} = remove_from_stack(Rest, From - 1, Count), {Removed, [CurStack|RestRemoved]}
	end.

add_to_stack(Stacks, To, ToAdd) -> 
	[CurStack | Rest] = Stacks,
	if
		To == 0 -> [(lists:reverse(ToAdd) ++ CurStack) | Rest];
		To > 0 -> [CurStack | add_to_stack(Rest, To - 1, ToAdd)]
	end.

apply_move_9000({Count, From, To}, Stacks) ->
	{Removed, RStacks} = remove_from_stack(Stacks, From, Count),
	add_to_stack(RStacks, To, Removed).

apply_move_9001({Count, From, To}, Stacks) ->
	{Removed, RStacks} = remove_from_stack(Stacks, From, Count),
	add_to_stack(RStacks, To, lists:reverse(Removed)).

first(Input) ->
	{Stacks, Moves} = parse(Input),
	Res = lists:foldl(fun apply_move_9000/2, Stacks, Moves),
	lists:map(fun hd/1, Res).

second(Input) ->
	{Stacks, Moves} = parse(Input),
	Res = lists:foldl(fun apply_move_9001/2, Stacks, Moves),
	lists:map(fun hd/1, Res).
