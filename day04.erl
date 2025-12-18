-module(day04).

main(_) ->
    {ok, Data} = file:read_file("data/day04.txt"),
    MapBitstrings = lists:droplast(string:split(Data, "\n", all)),
    MapStrings = lists:map(fun bitstring_to_list/1, MapBitstrings),
    MapLists = lists:map(fun as_chars/1, MapStrings),
    Map = array:from_list(lists:map(fun array:from_list/1, MapLists)),
    AccessableCount1 = count_accessables1(Map),
    io:format("Part One: ~B~n", [AccessableCount1]),
    AccessableCount2 = count_accessables2(Map),
    io:format("Part Two: ~B~n", [AccessableCount2]).

as_chars(String) -> [[C] || C <- String].

at(_, {-1, _}) -> undefined;
at(_, {_, -1}) -> undefined;
at(Map, {X, Y}) ->
    MaxX = array:size(array:get(0, Map)) - 1,
    MaxY = array:size(Map) - 1,
    case (X > MaxX) or (Y > MaxY) of
        true -> undefined;
        false -> array:get(Y, array:get(X, Map))
    end.

count_accessables1(Map) ->
    array:foldl(
        fun(X, Row, AccOuter) ->
            array:foldl(
                fun(Y, Element, InnerCount) ->
                    InnerCount + count_at_cell(Map, Element, {X, Y})
                end, AccOuter, Row
            )
        end, 0, Map
    ).

count_accessables2(Map) -> count_accessables2(Map, 0).

count_accessables2(Map, Count) ->
    {NewMap, NewCount} = array:foldl(
        fun(X, Row, {OuterMap, OuterCount}) ->
            update_row(Map, X, Row, OuterMap, OuterCount)
        end, {Map, Count}, Map
    ),
    case NewCount > Count of
        true -> count_accessables2(NewMap, NewCount);
        false -> NewCount
    end.

count_accessables(Map, Position) ->
    Neighbors = neighbors(Map, Position),
    NumAdjacent = length([N || N <- Neighbors, N == "@"]),
    check(NumAdjacent < 4, 1, 0).

count_at_cell(Map, "@", {X, Y}) -> count_accessables(Map, {X, Y});
count_at_cell(_, _, _) -> 0.

check(Pred, Pass, Fail) ->
    case Pred of
        true -> Pass;
        false -> Fail
    end.

neighbors(Map, {X, Y}) ->
    Offsets = [
        {DX, DY}
     || DX <- [-1, 0, 1],
        DY <- [-1, 0, 1],
        {DX, DY} =/= {0, 0}
    ],
    [
        V
     || {DX, DY} <- Offsets,
        V <- [at(Map, {X + DX, Y + DY})],
        V =/= undefined
    ].

update_cell(Map, "@", {X, Y}, InnerMap, InnerCount) ->
    AccCount = count_accessables(Map, {X, Y}),
    El = check(AccCount > 0, ".", "@"),
    NewInnerMap = array:set(Y, El, InnerMap),
    {NewInnerMap, InnerCount + AccCount};
update_cell(_, _, _, InnerMap, InnerCount) ->
    {InnerMap, InnerCount}.

update_row(Map, X, Row, OuterMap, OuterCount) ->
    {NewInnerMap, NewInnerCount} = array:foldl(
        fun(Y, Element, {InnerMap, InnerCount}) ->
            update_cell(Map, Element, {X, Y}, InnerMap, InnerCount)
        end, {OuterMap, OuterCount}, Row
    ),
    {array:set(X, NewInnerMap, OuterMap), NewInnerCount}.
