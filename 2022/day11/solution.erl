-module(solution).
-author("a-ungurianu").

%% API
-export([
	first/1,
	second/1,
	monkey/3
]).

-record(monkey, {id, items, operation, test, count}).

parse_op(<<"-">>) -> sub;
parse_op(<<"+">>) -> add;
parse_op(<<"*">>) -> mult.

parse_val(<<"old">>) -> self;
parse_val(ValS) -> binary_to_integer(ValS).


parse_operation(OperationS) ->
	Regex = "Operation: new = old ([-+*]) (old|\\d+)$",
	{match, [OpS, ValS]} = re:run(OperationS, Regex, [{capture, all_but_first, binary}]),
	{parse_op(OpS), parse_val(ValS)}.

parse_monkey_id(HeaderS) ->
	Regex = "Monkey (\\d+):",
	{match, [Id]} =  re:run(HeaderS, Regex, [{capture, all_but_first, binary}]),
	binary_to_integer(Id).

parse_starting_items(StartingItemsS) ->
	Regex = "Starting items: (.*)$",
	{match, [ItemsS]} = re:run(StartingItemsS, Regex, [{capture, all_but_first, binary}]),
	[ binary_to_integer(ItemS) || ItemS <- string:split(ItemsS, ", ", all)].

parse_test(TestS) ->
	Regex = "Test: divisible by (\\d+)",
	{match, [DivS]} = re:run(TestS, Regex, [{capture, all_but_first, binary}]),
	binary_to_integer(DivS).

parse_monkey_throw(ThrowS) -> 
	Regex = "throw to monkey (\\d+)",
	{match, [DestS]} = re:run(ThrowS, Regex, [{capture, all_but_first, binary}]),
	binary_to_integer(DestS).

parse_monkey([HeaderS, StartingItemsS, OperationS, TestS, TrueS, FalseS]) -> 
	parse_monkey([HeaderS, StartingItemsS, OperationS, TestS, TrueS, FalseS, ""]);
parse_monkey([HeaderS, StartingItemsS, OperationS, TestS, TrueS, FalseS, "" | Rest]) ->
	Id = parse_monkey_id(HeaderS),
	StartingItems = parse_starting_items(StartingItemsS),
	Operation = parse_operation(OperationS),
	Test = parse_test(TestS),
	TrueThrow = parse_monkey_throw(TrueS),
	FalseThrow = parse_monkey_throw(FalseS),
	{#monkey{id=Id, items=StartingItems, operation=Operation, test={Test, TrueThrow, FalseThrow}, count=0}, Rest}.

get_val(self, Item) -> Item;
get_val(Val, _) -> Val.

apply_op(Item, {add, Val}) -> (Item + get_val(Val, Item));
apply_op(Item, {mult, Val}) -> Item * get_val(Val, Item);
apply_op(Item, {sub, Val}) -> (Item - get_val(Val, Item)).

iff(Test, TrueV, FalseV) ->
	if
		Test -> TrueV;
		true -> FalseV
	end.

process_item(Item, #monkey{operation=Op, test={Div, TrueDest, FalseDest}}, WorryFactor, LimitFactor) ->
	ProcessedValue = (apply_op(Item, Op) div WorryFactor),
	ProcessedValueLimited = case LimitFactor of
		nolimit ->  ProcessedValue;
		Factor ->  ProcessedValue rem Factor 
	end,
	ThrowDest = iff(ProcessedValueLimited rem Div == 0, TrueDest, FalseDest),
	{ThrowDest, ProcessedValueLimited}.

process_items(#monkey{items=[]}, _, _) -> [];
process_items(#monkey{items=[Item | Rest], operation=Op, test={Div, TrueDest, FalseDest}} = MP, WorryFactor, LimitFactor) ->
	ProcessedValue = (apply_op(Item, Op) div WorryFactor),
	ProcessedValueLimited = case LimitFactor of
		nolimit ->  ProcessedValue;
		Factor ->  ProcessedValue rem Factor 
	end,
	ThrowDest = iff(ProcessedValueLimited rem Div == 0, TrueDest, FalseDest),
	ThrowItem = {ThrowDest, ProcessedValue},
	[ThrowItem | process_items(MP#monkey{items=Rest}, WorryFactor, LimitFactor)].

monkey(MonkeyProgram, WorryFactor, LimitFactor) ->
	receive 
		{monkeys, MonkeysPids} -> monkey(MonkeyProgram, WorryFactor, LimitFactor, MonkeysPids)
	end.
monkey(MonkeyProgram, WorryFactor, LimitFactor, MonkeysPids) ->
	receive
		{new_item, Item} -> 
			Items = MonkeyProgram#monkey.items ++ [process_item(Item, MonkeyProgram, WorryFactor, LimitFactor)],
			monkey(MonkeyProgram#monkey{items=Items}, WorryFactor, LimitFactor, MonkeysPids);
		throw_needed -> 
			ItemsToThrow = MonkeyProgram#monkey.items,
			lists:foreach(fun ({Dest, Value}) -> lists:nth(Dest + 1, MonkeysPids) ! {new_item, Value} end, ItemsToThrow),
			runner ! ok,
			monkey(MonkeyProgram#monkey{items=[], count=MonkeyProgram#monkey.count + length(ItemsToThrow)}, WorryFactor, LimitFactor, MonkeysPids);
		count ->
			runner ! {count, MonkeyProgram#monkey.count}
	end.	

collect_items(MonkeysToProcess) -> collect_items(MonkeysToProcess,MonkeysToProcess).
collect_items([], _) -> ok;
collect_items([MonkeyPid| Rest], Monkeys) ->
	MonkeyPid ! throw_needed,
	ok = receive
		ok -> ok
	end,
	collect_items(Rest, Monkeys).

get_counts(Monkeys) -> 
	lists:foreach(fun (Monkey) -> Monkey ! count end, Monkeys),
	lists:map(fun (_) -> receive {count, Count} -> Count end end, Monkeys).

runner(Monkeys) -> runner(Monkeys, 20).
runner(Monkeys, 0) -> get_counts(Monkeys);
runner(Monkeys, RunCount) ->
	ok = collect_items(Monkeys),
	runner(Monkeys, RunCount - 1).

parse([]) -> [];
parse(["\n"]) -> [];

parse(Input) ->
	{Monkey, Rest} = parse_monkey(Input),
	[Monkey | parse(Rest)].

get_factor([]) -> 1;
get_factor([#monkey{test={Div,_,_}}| Rest]) -> 
	Div * get_factor(Rest).



first(Input) ->
	Monkeys = parse(Input),
	register(runner, self()),
	MonkeyWorkers = lists:map(fun (MonkeyP) -> spawn(solution, monkey, [MonkeyP#monkey{items=process_items(MonkeyP, 3, nolimit)}, 3, nolimit]) end, Monkeys),
	lists:foreach(fun(MonkeyPid) -> MonkeyPid ! {monkeys, MonkeyWorkers} end, MonkeyWorkers),
	[T1, T2 | _] = lists:sort(fun (X,Y) -> X > Y end, runner(MonkeyWorkers)),
	T1 * T2.

second(Input) ->
	Monkeys = parse(Input),
	Factor = get_factor(Monkeys),
	MonkeyWorkers = lists:map(fun (MonkeyP) -> spawn(solution, monkey, [MonkeyP#monkey{items=process_items(MonkeyP, 1, Factor)}, 1, Factor]) end, Monkeys),
	lists:foreach(fun(MonkeyPid) -> MonkeyPid ! {monkeys, MonkeyWorkers} end, MonkeyWorkers),
	[T1, T2 | _] = lists:sort(fun (X,Y) -> X > Y end, runner(MonkeyWorkers, 10000)),
	T1 * T2.
