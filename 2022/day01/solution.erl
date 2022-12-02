-module(solution).
-author("hypothermic").

%% API
-export([
	first/1,
	second/1
]).

split_into_lists(Xs) -> split_into_lists(Xs, []).

split_into_lists([], Acc) -> [Acc];
split_into_lists([X|Xs], Acc) ->
	case X of
		"" -> [Acc|split_into_lists(Xs, [])];
		_ -> split_into_lists(Xs, [X|Acc])
	end.

int(L) -> 
	case string:list_to_integer(L) of
		{I, []} -> I
	end.

int_list(Ls) -> lists:map(fun int/1, Ls).

elf_counts(Elf_bags) -> lists:map(fun lists:sum/1, Elf_bags).

parse(Input) -> lists:map(fun int_list/1, split_into_lists(Input)).


first(Input) ->
	lists:max(elf_counts(parse(Input))).

second(Input) ->
	lists:sum(lists:sublist(lists:sort(fun(A,B) -> A > B end, elf_counts(parse(Input))), 3)).
