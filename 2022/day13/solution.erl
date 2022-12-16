-module(solution).
-author("a-ungurianu").

%% API
-export([
	first/1,
	second/1
]).

parse_number(Line) ->
	case re:split(Line, "(\\d+)", [{return, list}, {parts, 2}]) of
		[[], Num, Rest] -> {list_to_integer(Num), Rest};
		_ -> invalid
	end.

parse_value(Line) -> 
	Ret = case parse_list(Line) of
		invalid -> parse_number(Line);
		Res -> Res
	end,
	Ret.

parse_elements("]" ++ Rem) -> {[], "]" ++ Rem};
parse_elements(List) -> 
	Ret = case parse_value(List) of
		{Value, "," ++ Rem} -> case parse_elements(Rem) of
			{RestElements, Rema} -> {[Value | RestElements], Rema};
			_ -> invalid
			end;
		{Value, Rem} -> {[Value], Rem}
	end,
	Ret.
			

parse_list("[" ++ Els) ->
	case parse_elements(Els) of
		{Elements, "]" ++ Rem} ->
			{Elements, Rem};
		_ -> invalid
	end;
parse_list(_) -> invalid.

parse_packet(Line) ->
	{Packet, []} = parse_list(Line),
	Packet.

parse_pairs([]) -> [];
parse_pairs([First, Second, "" | Rest]) ->
	[[parse_packet(First), parse_packet(Second)] | parse_pairs(Rest)].

parse(Input) ->
	parse_pairs(Input).

compare_integers(A,B) ->
	if 
		A < B -> less;
		A == B -> equal;
		A > B -> greater
	end.

packet_el_less(A, B) when is_integer(A) and is_integer(B) -> compare_integers(A,B);
packet_el_less(A, B) when is_integer(A) and is_list(B) -> compare_packet([A],B);
packet_el_less(A, B) when is_list(A) and is_integer(B) -> compare_packet(A,[B]);
packet_el_less(A, B) when is_list(A) and is_list(B) -> compare_packet(A,B).

compare_packet([], []) -> equal;
compare_packet([], _) -> less;
compare_packet(_, []) -> greater;
compare_packet([A|As], [B|Bs]) -> 
	case packet_el_less(A,B) of
		equal -> compare_packet(As, Bs);
		S -> S
	end.

first(Input) ->
	Data = parse(Input),
	lists:sum(lists:map(fun ({Idx, _}) -> Idx end, lists:filter(fun ({_, [A,B]}) -> compare_packet(A,B) /= greater end, lists:enumerate(Data)))).

print_lines([]) -> ok;
print_lines([Line | Lines]) ->
	io:format("~w\n", [Line]),
	print_lines(Lines).

second(Input) ->
	Data = lists:merge(parse(Input)),
	S1 = [[2]],
	S2 = [[6]],
	Packets = [S1, S2 | Data],
	Sorted = lists:sort(fun (A,B) -> compare_packet(A,B) == less end, Packets),
	[{Idx1, _}, {Idx2, _}] = lists:filter(fun ({_, X}) -> (X == S1) or (X == S2) end, lists:enumerate(Sorted)),
	Idx1 * Idx2.
