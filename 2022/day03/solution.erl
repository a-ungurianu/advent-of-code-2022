-module(solution).
-author("a-ungurianu").

%% API
-export([
	first/1,
	second/1
]).

split_into_compartements(Line) ->
	CompartementSize = erlang:length(Line) div 2,
	lists:split(CompartementSize, Line).

parse1(Input) -> [split_into_compartements(Line) || Line <- Input].

get_priority(C) -> 
	if
		$a =< C, C =< $z -> C - $a + 1; 
		$A =< C, C =< $Z -> C - $A + 27
	end.

find_intersect(Compartements) -> 
	{Compartement1, Compartement2} = Compartements,
	C1_Set = sets:from_list(Compartement1),
	C2_Set = sets:from_list(Compartement2),
	Both = sets:intersection(C1_Set, C2_Set),
	lists:last(sets:to_list(Both)).

first(Input) ->
	Backpacks = parse1(Input),
	lists:sum([get_priority(find_intersect(Backpack)) || Backpack <- Backpacks]).

group_in_3s(List) ->
	case List of
		[A,B,C] -> [[A,B,C]];
		[A,B,C|Rest] -> [[A,B,C] | group_in_3s(Rest)]
	end.


second(Input) ->
	ElfGroups = group_in_3s(lists:map(fun sets:from_list/1, Input)), 
	GroupBadges = lists:map(fun sets:intersection/1, ElfGroups),
	lists:sum([get_priority(lists:last(sets:to_list(GroupBadge))) || GroupBadge <- GroupBadges]).
