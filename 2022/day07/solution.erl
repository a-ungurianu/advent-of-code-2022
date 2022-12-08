-module(solution).
-author("a-ungurianu").

%% API
-export([
	first/1,
	second/1
]).

parse_ls_line(Line) ->
	[L,R] = string:split(Line, " "),
	case {L,R} of
		{"dir", Name} -> {dir, Name};
		{Size, Name} -> {file, Name, list_to_integer(Size)}
	end.

parse_ls_output(Input) -> 
	LsLines = lists:takewhile(fun (L) -> not (hd(L) == $$) end, Input),
	Rest = lists:nthtail(length(LsLines), Input),
	{lists:map(fun parse_ls_line/1, LsLines), Rest}.

parse_prompt("$ " ++ Line) ->
	case Line of
		"ls" -> {ls};
		"cd " ++ Dir -> {cd, Dir}
	end.


parse_command(Input) ->
	Propmt = parse_prompt(hd(Input)),
	case Propmt of
		{ls} -> {Output, Rest} = parse_ls_output(tl(Input)), {{ls, Output}, Rest};
		{cd, Name} -> {{cd, Name}, tl(Input)}
	end.

parse([]) -> [];
parse(Input) -> 
	{Command, Rest} = parse_command(Input),
	[Command | parse(Rest)].

navigate(CurDir, Where) ->
	case Where of
		"/" -> [];
		".." -> tl(CurDir);
		Wh -> [Wh| CurDir]
	end.

add_file_to_fs(Fs, [], {dir, Name}) -> Fs#{Name => #{}};
add_file_to_fs(Fs, [], {file, Name, Size}) -> Fs#{Name => Size};
add_file_to_fs(Fs, Path, File) -> Fs#{
	hd(Path) => add_file_to_fs(maps:get(hd(Path), Fs, #{}), tl(Path), File)
	}.


update_fs_from_ls(Fs, _, []) -> Fs;
update_fs_from_ls(Fs, CurDir, [File | Rest]) -> update_fs_from_ls(add_file_to_fs(Fs, CurDir, File), CurDir, Rest).

build_fs(Commands) -> build_fs(Commands, {[], #{}}).
build_fs([], {_CurDir, Fs}) -> Fs;
build_fs([Command|Rest], {CurDir, Fs}) -> 
	case Command of 
		{cd, Name} -> build_fs(Rest, {navigate(CurDir, Name), Fs});
		{ls, Listing} -> build_fs(Rest, {CurDir, update_fs_from_ls(Fs, lists:reverse(CurDir), Listing)})
	end.


sum_fs_l([], DirSizes) -> {0, DirSizes};
sum_fs_l([File| Rest], DirSizes) ->
	{RestSize, RestDirSizes} = sum_fs_l(Rest, DirSizes),
	if 
		is_integer(File) -> {File + RestSize, RestDirSizes};
		is_map(File) -> {Size, DirDirSizes} = sum_fs(File), { Size + RestSize, DirDirSizes ++ RestDirSizes}
	end.

sum_fs(Fs) -> sum_fs(Fs, []).
sum_fs(Fs, DirSizes) ->
	{Size, RestDirSizes} = sum_fs_l(maps:values(Fs), DirSizes),
	{Size, [Size | RestDirSizes]}.


process_input(Input) ->
	Data = parse(Input),
	Fs = build_fs(Data),
	sum_fs(Fs).
	

first(Input) ->
	{_RootSize, AllSizes} = process_input(Input),
	lists:sum(lists:filter(fun(X) -> X =< 100000 end, AllSizes)).

second(Input) ->
	{RootSize, AllSizes} = process_input(Input),
	AmountAvailable = 70000000 - RootSize,
	AmountToFree = 30000000 - AmountAvailable,
	lists:min(lists:filter(fun(X) -> X >= AmountToFree end, AllSizes)).
