-module(solution).
-author("a-ungurianu").

%% API
-export([
	first/1,
	second/1
]).


parse_operator("+") -> add;
parse_operator("-") -> sub;
parse_operator("/") -> idiv;
parse_operator("*") -> mul.

parse_op(OpS) ->
	Regex = "^(?:(\\d+)|(?:(\\w+) ([+/*-]) (\\w+)))$",
	case re:run(OpS, Regex,[{capture, all_but_first, list}]) of
		{match, [[], LhsS, Operator, RhsS]} -> {parse_operator(Operator), LhsS, RhsS};
		{match, [ConstantS]} -> {constant, list_to_integer(ConstantS)}
	end.

parse_monkey(Line) -> 
	[Name, OpS] = string:split(Line, ": "),
	{Name, parse_op(OpS)}.


parse([]) -> #{};
parse([Line | Rest]) -> 
	{Name, Op} = parse_monkey(Line),
	Res = parse(Rest),
	Res#{
		Name => Op
	}.

op(add, Lhs, Rhs) -> Lhs + Rhs;
op(sub, Lhs, Rhs) -> Lhs - Rhs;
op(idiv, Lhs, Rhs) -> Lhs div Rhs;
op(mul, Lhs, Rhs) -> Lhs * Rhs;
op(eq, Lhs, Rhs) -> Lhs == Rhs.

eval(Tree) -> eval(Tree, "root").
eval(Tree, Key) ->
	#{Key := Op} = Tree,
	case Op of
		{constant, Val} -> Val;
		{Operator, LhsP, RhsP} ->  
			Lhs = eval(Tree, LhsP),
			Rhs = eval(Tree, RhsP),
			op(Operator, Lhs, Rhs)
	end.


first(Input) ->
	Tree = parse(Input),
	eval(Tree).

sgn(X) when X == 0 -> 0;
sgn(X) when X > 0 -> 1;
sgn(X) when X < 0 -> -1.

patch_candidate(Tree, Candidate) ->
	Tree#{
		"humn" => {constant, Candidate}
	}.

find_solution(Tree) -> find_solution(Tree, 0, 1, 0).
find_solution(Tree, Candidate,  Step, LastLhs) ->
	io:format("\nCandidate: ~w\n", [Candidate]),
	TreeWithCandidate =  patch_candidate(Tree, Candidate),
	#{"root" := {eq, LhsP, RhsP}} = TreeWithCandidate,
	Lhs = eval(TreeWithCandidate, LhsP),
	Rhs = eval(TreeWithCandidate, RhsP),

	io:format("Root: Lhs=~w, Rhs=~w, LastLhs=~w\n", [Lhs, Rhs, LastLhs]),

	% This is lucky, as this could very easily get stuck in a local minima :)
	if 
		Lhs == Rhs -> Candidate;
		LastLhs >= Lhs, Lhs > Rhs -> find_solution(Tree, Candidate + Step, Step * 2, Lhs); 
		LastLhs > Lhs, Lhs < Rhs -> find_solution(Tree, Candidate - sgn(Step), -sgn(Step), Lhs);
		Rhs > Lhs, Lhs >= LastLhs -> find_solution(Tree, Candidate + Step, Step * 2, Lhs); 
		Rhs < Lhs, Lhs > LastLhs -> find_solution(Tree, Candidate - sgn(Step), -sgn(Step), Lhs)
	end.

find_lowest(Tree, Candidate) ->
	TreeWithCandidate = patch_candidate(Tree, Candidate),
	TreeWithCandidateEval = eval(TreeWithCandidate),
	case TreeWithCandidateEval of
		true -> find_lowest(Tree, Candidate - 1);
		false -> Candidate + 1
	end.

second(Input) ->
	Tree = parse(Input),
	PatchedTree = maps:update_with("root", fun ({_, Lhs, Rhs}) -> {eq, Lhs, Rhs} end, Tree),
	FirstCandidate = find_solution(PatchedTree),
	find_lowest(PatchedTree, FirstCandidate).
