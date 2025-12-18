-module(day03).

main(_) ->
    {ok, Data} = file:read_file("data/day03.txt"),
    RatingStrings = lists:droplast(string:split(Data, "\n", all)),
    Ratings = lists:map(fun to_digits/1, RatingStrings),
    LargestJoltages1 = lists:map(fun largest_joltage1/1, Ratings),
    io:format("Part One: ~B~n", [lists:sum(LargestJoltages1)]),
    LargestJoltages2 = lists:map(fun largest_joltage2/1, Ratings),
    io:format("Part Two: ~B~n", [lists:sum(LargestJoltages2)]).

index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _) -> not_found;
index_of(Item, [Item | _], Index) -> Index;
index_of(Item, [_ | Tail], Index) -> index_of(Item, Tail, Index + 1).

largest_joltage1(Rating) ->
    Tens = lists:max(lists:droplast(Rating)),
    Index = index_of(Tens, Rating),
    {_, Rest} = lists:split(Index, Rating),
    Ones = lists:max(Rest),
    (Tens * 10) + Ones.

largest_joltage2(Rating) ->
    Digits = lists:reverse(largest_joltage2(Rating, 12, [])),
    List = lists:map(fun integer_to_list/1, Digits),
    list_to_integer(lists:flatten(lists:join("", List))).

largest_joltage2(_, 0, Digits) ->
    Digits;
largest_joltage2(Rating, Num, Digits) ->
    {Sub, _} = lists:split(length_of(Rating) - Num + 1, Rating),
    Digit = lists:max(Sub),
    Index = index_of(Digit, Rating),
    {_, Rest} = lists:split(Index, Rating),
    largest_joltage2(Rest, Num - 1, [Digit | Digits]).

length_of(List) -> length_of(List, 0).

length_of([], Len) -> Len;
length_of([_ | Rest], Len) -> length_of(Rest, Len + 1).

to_digits(String) ->
    [list_to_integer([C]) || C <- string:to_graphemes(String)].
