-module(solution).
-author("a-ungurianu").

%% API
-export([
	first/1,
	second/1
]).

parse_sensor(Line) ->
	Regex = "Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)",
	case re:run(Line, Regex, [{capture, all_but_first, binary}]) of
		{match, Numbers} ->
			[SX, SY, CBX, CBY] = lists:map(fun binary_to_integer/1, Numbers),
			{{SX, SY}, {CBX, CBY}}
	end.

parse(Input) ->
	lists:map(fun parse_sensor/1, Input).

m_dist({X1, Y1}, {X2, Y2}) -> abs(X1 - X2) + abs(Y1 - Y2).

sensor_range_to_point_map(_, 0, _) -> #{};
sensor_range_to_point_map({SX, SY}=SP, Range, Idx) ->
	Inner = sensor_range_to_point_map(SP, Range - 1, Idx),
	PointsInRangeTopLeft = [{SX - (Range - I), SY + I} || I <- lists:seq(0, Range)],
	PointsInRangeTopRight = [{SX + (Range - I), SY + I} || I <- lists:seq(0, Range)],
	PointsInRangeBottomLeft = [{SX - (Range - I), SY - I} || I <- lists:seq(0, Range)],
	PointsInRangeBottomRight = [{SX + (Range - I), SY - I} || I <- lists:seq(0, Range)],
	Points = PointsInRangeTopLeft ++ PointsInRangeTopRight ++ PointsInRangeBottomLeft ++ PointsInRangeBottomRight,
	maps:merge(Inner, maps:from_keys(Points, [$0 + Idx])).

paint_sensor_ranges([]) -> #{};
paint_sensor_ranges([{Idx, {SP, CBP}}| Rest]) ->
	SensorRange = m_dist(SP, CBP),
	Res = sensor_range_to_point_map(SP, SensorRange, Idx),
	maps:merge(paint_sensor_ranges(Rest), Res).

paint_features(Detections) ->
	Sensors = [SP || {SP, _} <- Detections],
	Beacons = [BP || {_, BP} <- Detections],
	maps:merge(maps:from_keys(Sensors, "\033[31;1mS\033[0m"), maps:from_keys(Beacons, "\033[32;1mB\033[0m")).

to_pretty_map(Detections) ->
	SensorRange = paint_sensor_ranges(lists:enumerate(Detections)),
	Features = paint_features(Detections),
	maps:merge(SensorRange, Features).


find_safe_at_y([], _) -> [];
find_safe_at_y([{{SX, SY} = SP, CBP} | Rest], Y) -> 
	SensorRange = m_dist(SP, CBP),
	YDistToSensor = abs(SY-Y),
	LeftOverRange = max(SensorRange - YDistToSensor, 0),
	if 
		LeftOverRange > 0 -> [{SX-LeftOverRange, a_open}, {SX + LeftOverRange, b_close} | find_safe_at_y(Rest, Y)];
		true -> find_safe_at_y(Rest, Y)
	end.

count_from_intervals(Edges) -> count_from_intervals(Edges, 0, none).
count_from_intervals([], 0, none) -> 0;
count_from_intervals([{Y, a_open}| Rest], 0, none) -> count_from_intervals(Rest, 1, Y);
count_from_intervals([{Y, b_close}| Rest], 1, PrevY) -> 
	% io:format("PrevY=~w, Y=~w\n", [PrevY, Y]),
	(Y - PrevY) + count_from_intervals(Rest, 0, none);
count_from_intervals([{_, a_open}| Rest], OpenCount, PrevY) -> count_from_intervals(Rest, OpenCount + 1, PrevY);
count_from_intervals([{_, b_close}| Rest], OpenCount, PrevY) when OpenCount > 0 -> count_from_intervals(Rest, OpenCount - 1, PrevY).

first(Input) ->
	Data = parse(Input),
	% io:format("Data=~w\n", [Data]),
	% Map = to_pretty_map(Data),
	% io:format("Map\n~s\n", [map_utils:map_to_string(Map, {-2,-2}, {25,25})]),
	SafeYs = find_safe_at_y(Data, 2000000),
	% io:format("SafeYs=~w\n", [lists:sort(SafeYs)]),
	count_from_intervals(lists:sort(SafeYs)).

second(Input) ->
	0.
