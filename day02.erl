-module(day02).

main(_) ->
    {ok, Data} = file:read_file("day02.txt"),
    RangesString = lists:droplast(bitstring_to_list(Data)),
    RangeStrings = string:split(RangesString, ",", all),
    Ranges = lists:map(fun to_range/1, RangeStrings),
    IncorrectNumbers1 = lists:flatmap(fun incorrect_numbers1/1, Ranges),
    io:format("Part One: ~p~n", [lists:sum(IncorrectNumbers1)]),
    IncorrectNumbers2 = lists:flatmap(fun incorrect_numbers2/1, Ranges),
    io:format("Part Two: ~p~n", [lists:sum(IncorrectNumbers2)]).

divide(N, String) ->
    Length = string:length(String),
    if
        Length < N ->
            String;
        true ->
            {Segment, Rest} = lists:split(N, String),
            [Segment | divide(N, Rest)]
    end.

incorrect_numbers1({Min, Max}) ->
    Numbers = lists:seq(Min, Max),
    lists:filter(fun is_incorrect1/1, Numbers).

incorrect_numbers2({Min, Max}) ->
    Numbers = lists:seq(Min, Max),
    lists:filter(fun is_incorrect2/1, Numbers).

is_incorrect1(Number) ->
    String = integer_to_list(Number),
    is_mirrored(String).

is_incorrect2(Number) ->
    String = integer_to_list(Number),
    is_repeated(1, String).

is_mirrored(String) when (length(String) rem 2) == 1 -> false;
is_mirrored(String) ->
    Midpoint = floor(string:length(String) / 2),
    {First, Second} = lists:split(Midpoint, String),
    First == Second.

is_repeated(N, String) when N > (length(String) / 2) -> false;
is_repeated(N, String) ->
    [First | Substrings] = divide(N, String),
    Repeats = lists:all(fun(X) -> X == First end, Substrings),
    if
        Repeats -> true;
        true -> is_repeated(N + 1, String)
    end.

to_range(RangeString) ->
    [MinString, MaxString] = string:split(RangeString, "-"),
    {Min, _} = string:to_integer(MinString),
    {Max, _} = string:to_integer(MaxString),
    {Min, Max}.
