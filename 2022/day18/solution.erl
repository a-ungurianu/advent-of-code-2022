-module(solution).
-author("a-ungurianu").

%% API
-export([
	first/1,
	second/1
]).

-define(DIR_OFFS, [{0,0,1},{0,0,-1},{0,1,0},{0,-1,0},{1,0,0},{-1,0,0}]).

add3({X1,Y1,Z1},{X2,Y2,Z2}) ->
	{X1 + X2, Y1 + Y2, Z1 + Z2}.

parse_3d_point(Line) ->
	[X,Y,Z] = lists:map(fun list_to_integer/1, string:split(Line, ",", all)),
	{X,Y,Z}.

parse(Input) -> 
	lists:map(fun parse_3d_point/1, Input).

count_surface_faces([], _) -> 0;
count_surface_faces([Point| Rest], PointsSet) ->
	Neighs = lists:map(fun(Dir) -> add3(Point, Dir) end, ?DIR_OFFS),
	FreeNeights = lists:filter(fun (Neigh) -> not sets:is_element(Neigh, PointsSet) end, Neighs),
	length(FreeNeights) + count_surface_faces(Rest, PointsSet).

first(Input) ->
	Points = parse(Input),
	% io:format("Points=~w\n", [Points]),
	count_surface_faces(Points, sets:from_list(Points)).

second(Input) ->
	0.
