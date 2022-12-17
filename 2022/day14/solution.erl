-module(solution).
-author("a-ungurianu").

%% API
-export([
	first/1,
	second/1
]).

parse_point(Str) ->
	[X, Y] = lists:map(fun list_to_integer/1, string:split(Str, ",")),
	{X, Y}.

parse_line(Line) -> 
	Points = string:split(Line, " -> " , all),
	lists:map(fun parse_point/1, Points).

parse_lines(Input) -> lists:map(fun parse_line/1, Input).


left({X,Y}) -> {X - 1, Y}.
right({X,Y}) -> {X + 1, Y}.
up({X,Y}) -> {X, Y - 1}.
down({X,Y}) -> {X, Y + 1}.

dir_between_points({X1,Y1}, {X2, Y2}) ->
	if 
		X1 < X2 -> fun right/1;
		X1 > X2 -> fun left/1;
		Y1 < Y2 -> fun down/1;
		Y1 > Y2 -> fun up/1
	end.

segment_to_map(P,P) -> #{P => wall};
segment_to_map(P1, P2) -> 
	DirF = dir_between_points(P1, P2),
	Res = segment_to_map(DirF(P1), P2),
	Res#{
		P1 => wall
		}.

line_to_map([P1, P2 | Rest]) -> maps:merge(line_to_map([P2 | Rest]), segment_to_map(P1, P2));
line_to_map(_) -> #{}.

lines_to_map([]) -> #{};
lines_to_map([Line | Rest]) -> maps:merge(line_to_map(Line), lines_to_map(Rest)). 

map_el_to_c(wall) -> "#";
map_el_to_c(sand) -> "o".

is_occupied(_, {_,Y}, Floor) when Y >= Floor + 2 -> true;
is_occupied(PointsMap, Pos, _) -> 
	case PointsMap of
		#{Pos := _} -> true;
		#{} -> false
	end.

step_sand({_, Y}, PointsMap, SimLimit, _) when Y > SimLimit -> PointsMap;
step_sand(SandPos, PointsMap, SimLimit, InfiniteFloor) ->
	Down = down(SandPos),
	DownLeft = left(down(SandPos)),
	DownRight = right(down(SandPos)),
	case lists:filter(fun (P) -> not is_occupied(PointsMap, P, InfiniteFloor) end, [Down, DownLeft, DownRight]) of
		[] -> PointsMap#{SandPos => sand};
		[AvailablePos| _] -> step_sand(AvailablePos, PointsMap, SimLimit, InfiniteFloor)
	end.

sim(PointsMap, SimLimit, InfiniteFloor) ->
	NewMap = step_sand({500, 0}, PointsMap, SimLimit, InfiniteFloor),
	OldMapSize = maps:size(PointsMap),
	NewMapSize = maps:size(NewMap),
	if 
		OldMapSize == NewMapSize -> NewMap;
		true -> sim(NewMap, SimLimit, InfiniteFloor)
	end.

find_floor(Lines) ->
	Points = lists:merge(Lines),
	lists:max(lists:map(fun ({_,Y}) -> Y end, Points)).
 
print_sand_map(PointsMap) ->
	CharMap = maps:map(fun (_, V) -> map_el_to_c(V) end, PointsMap),
	map_utils:map_to_string(CharMap, {490, 0}, {510, 13}).

first(Input) ->
	Lines = parse_lines(Input),
	Map = lines_to_map(Lines),
	Floor = find_floor(Lines),
	Grained = sim(Map, Floor, Floor + 2),
	maps:size(Grained) - maps:size(Map).

second(Input) ->
	Lines = parse_lines(Input),
	Map = lines_to_map(Lines),
	Floor = find_floor(Lines),
	Grained = sim(Map, Floor + 1, Floor),
	maps:size(Grained) - maps:size(Map).
