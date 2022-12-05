-module(solution).
-author("a-ungurianu").

%% API
-export([
	first/1,
	second/1
]).

split_into_compartements(Line) ->
	CompartementSize = erlang:length(Line) div 2,
	tuple_to_list(lists:split(CompartementSize, Line)).

parse1(Input) -> lists:map(fun split_into_compartements/1, Input).

get_priority(C) -> 
	if
		$a =< C, C =< $z -> C - $a + 1; 
		$A =< C, C =< $Z -> C - $A + 27
	end.

find_intersect(Compartements) -> 
	Both = sets:intersection(lists:map(fun sets:from_list/1, Compartements)),
	hd(sets:to_list(Both)).

first(Input) ->
	Backpacks = parse1(Input),
	lists:sum([get_priority(find_intersect(Backpack)) || Backpack <- Backpacks]).

group_in_3s([]) -> [];
group_in_3s(Ls) -> {Group, Rest} = lists:split(3, Ls), [Group | group_in_3s(Rest)].


second(Input) ->
	ElfGroups = group_in_3s(lists:map(fun sets:from_list/1, Input)), 
	GroupBadges = lists:map(fun sets:intersection/1, ElfGroups),
	lists:sum([get_priority(lists:last(sets:to_list(GroupBadge))) || GroupBadge <- GroupBadges]).
