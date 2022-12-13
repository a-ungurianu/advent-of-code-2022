-module(solution).
-author("a-ungurianu").

%% API
-export([
	first/1,
	second/1
]).

parse_instruction(Line) -> 
	case string:split(Line, " ", all) of
		["noop"] -> {noop};
		["addx", Amount] -> {addx, list_to_integer(Amount)}
		end.

parse(Input) -> lists:map(fun parse_instruction/1, Input).

instructions_to_signal(Instructions) -> instructions_to_signal(Instructions, 1).
instructions_to_signal([], _Strength) -> [];
instructions_to_signal([Instruction | Rest], Strength) ->
	case Instruction of
		{noop} -> [Strength| instructions_to_signal(Rest, Strength)];
		{addx, Amount} ->[Strength, Strength | instructions_to_signal(Rest, Strength + Amount)]
	end.

find_strength([], _, _) -> 0;
find_strength([Power|Rest], Seen, Seen) -> 
	io:format("Sample point [~w]: ~w\n", [Seen, Power]),
	(Power * Seen) + find_strength(Rest, Seen + 1, Seen + 40);
find_strength([], _, 260) -> 0;
find_strength([_|Rest], Idx, Seen) -> find_strength(Rest, Idx + 1, Seen).

first(Input) ->
	Instructions = parse(Input),
	Signals = instructions_to_signal(Instructions),
	Strength = find_strength(Signals, 1, 20),
	integer_to_list(Strength).

to_crt([]) -> "";
to_crt([{CycleCount, SpriteX} | Rest]) ->
	ScreenX = (CycleCount - 1) rem 40,
	Pixel = if
		abs(ScreenX - SpriteX) < 2 -> "#";
		true -> "."
	end,
	if 
		ScreenX == 0 -> "\n" ++ Pixel;
		true -> Pixel
	end ++ to_crt(Rest);
to_crt(Signals) -> to_crt(lists:enumerate(Signals)).

second(Input) ->
	Instructions = parse(Input),
	Signals = instructions_to_signal(Instructions),
	Screen = to_crt(Signals),
	io:format("Screen\n~s\n",[Screen]), "See Terminal Output".
