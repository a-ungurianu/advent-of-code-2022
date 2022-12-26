-module(solution).
-author("a-ungurianu").

%% API
-export([
	first/1,
	second/1
]).

snafu_digit_to_dec($2) -> 2;
snafu_digit_to_dec($1) -> 1;
snafu_digit_to_dec($0) -> 0;
snafu_digit_to_dec($-) -> -1;
snafu_digit_to_dec($=) -> -2.

snafu_to_dec(Line) -> snafu_to_dec(Line, 0).
snafu_to_dec([], Acc) -> Acc;
snafu_to_dec([Digit| Rest], Acc) ->
	snafu_to_dec(Rest, Acc * 5 + snafu_digit_to_dec(Digit)).

dec_to_snafu(0) -> "";
dec_to_snafu(Number) ->
	case Number rem 5 of
		0 -> dec_to_snafu(Number div 5) ++ "0";
		1 -> dec_to_snafu((Number - 1) div 5) ++ "1";
		2 -> dec_to_snafu((Number - 2) div 5) ++ "2";
		3 -> dec_to_snafu((Number + 2) div 5) ++ "=";
		4 -> dec_to_snafu((Number + 1) div 5) ++ "-"
	end.


parse(Input) -> lists:map(fun snafu_to_dec/1, Input).

first(Input) ->
	Numbers = parse(Input),
	dec_to_snafu(lists:sum(Numbers)).

second(Input) ->
	"".
