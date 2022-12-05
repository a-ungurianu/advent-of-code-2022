-module(solution).
-author("a-ungurianu").

%% API
-export([
	first/1,
	second/1
]).

parse_int(S) -> 
	case string:to_integer(S) of
		{I, []} -> I
	end.

parse_interval(IntS) -> 
	[L, R] = lists:map(fun parse_int/1, string:split(IntS, "-")),
	{L,R}.


parse_row(Input) ->
	Elfs = string:split(Input,","),
	lists:map(fun parse_interval/1, Elfs).

parse(Input) -> lists:map(fun parse_row/1, Input).

is_fully_overlapping(I1, I2) ->
	{L1, R1} = I1,
	{L2, R2} = I2,
	if 
		(L1 =< L2) and (R2 =< R1) -> true;
		(L2 =< L1) and (R1 =< R2) -> true;
		true -> false
	end.

solution(Input, Filter) ->
	Data = parse(Input),
	% io:format("Parsed: ~w\n", [Data]),
	Filtered = lists:filter(fun ([I1, I2]) -> Filter(I1, I2) end, Data),
	% io:format("Filtered: ~w\n", [Filtered]),
	length(Filtered).

first(Input) -> solution(Input, fun is_fully_overlapping/2).

is_partially_overlapping(I1, I2) ->
	{L1, R1} = I1,
	{L2, R2} = I2,
	if 
		% 1:    L-----R
		% 2:        L-----R
		(L1 =< L2) and (L2 =< R1) -> true;
		% 1:        L-----R
		% 2:     L-----R
		(L2 =< L1) and (L1 =< R2) -> true;
		true -> false
	end.

second(Input) -> solution(Input, fun is_partially_overlapping/2).
