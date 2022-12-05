-module(solution).
-author("a-ungurianu").

%% API
-export([
	first/1,
	second/1
]).


col_a_to_symbol("A") -> rock;
col_a_to_symbol("B") -> paper;
col_a_to_symbol("C") -> scissors.

col_b_to_symbol("X") -> rock;
col_b_to_symbol("Y") -> paper;
col_b_to_symbol("Z") -> scissors.

losing_piece(rock) -> scissors;
losing_piece(scissors) -> paper;
losing_piece(paper) -> rock.

winning_piece(rock) -> paper;
winning_piece(paper) -> scissors;
winning_piece(scissors) -> rock.

parse_row(Row) -> 
	[A,B] = string:split(Row, " "), 
	{col_a_to_symbol(A), B}.

symbol_to_score(rock) -> 1;
symbol_to_score(paper) -> 2;
symbol_to_score(scissors) -> 3.

outcome_score(Round) ->
	{Opp,Me} = Round, 
	LosingPiece = losing_piece(Opp),
	if
		Opp == Me -> 3; % Tie
		Me == LosingPiece -> 0; % I lose
		true -> 6 % I wina
	end.

decode_round1(Round) -> 
	{Opp, Me_s} = Round,
	{Opp, col_b_to_symbol(Me_s)}.

calculate_score(Round) ->
	{_,Me} = Round,
	symbol_to_score(Me) + outcome_score(Round).

parse(Input) -> [parse_row(Row) || Row <- Input].

first(Input) ->
	lists:sum([calculate_score(decode_round1(Round)) || Round <- parse(Input)]).

col_b_to_outcome("X") -> lose;
col_b_to_outcome("Y") -> draw;
col_b_to_outcome("Z") -> win.

decode_round2(Round) -> 
	{Opp, Outcome_s} = Round,
	{Opp, col_b_to_outcome(Outcome_s)}.

outcome_to_game(G) ->
	case G of
		{Opp, draw} -> {Opp, Opp};
		{Opp, lose} -> {Opp, losing_piece(Opp)};
		{Opp, win} -> {Opp, winning_piece(Opp)}
	end.


second(Input) ->
	lists:sum([calculate_score(outcome_to_game(decode_round2(Round))) || Round <- parse(Input)]).
