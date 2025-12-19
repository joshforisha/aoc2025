-module(day05).

main(_) ->
    {ok, Data} = file:read_file("data/day05.txt"),
    [FreshRangesStr | AvailableIdsStr] = string:split(Data, "\n\n"),
    FreshRanges = fresh_ranges(FreshRangesStr),
    AvailableIds = available_ids(AvailableIdsStr),
    IsFresh = fun(Id) ->
        lists:any(fun(R) -> within(Id, R) end, FreshRanges)
    end,
    FreshIngredients = lists:filter(IsFresh, AvailableIds),
    io:format("Part One: ~B~n", [length(FreshIngredients)]),
    io:format("Part Two: ~B~n", [count_range_ids(FreshRanges)]).

available_ids(IdsString) ->
    IdStrings = lists:droplast(string:split(IdsString, "\n", all)),
    lists:map(fun to_int/1, IdStrings).

count_range_ids(RangesList) ->
    lists:foldl(
        fun({Min, Max}, Total) ->
            Total + (Max - Min) + 1
        end,
        0,
        RangesList
    ).

fresh_ranges(RangesString) ->
    RangeStrings = string:split(RangesString, "\n", all),
    reduce_ranges(lists:map(fun to_range/1, RangeStrings)).

reduce_ranges(RangesList) ->
    RangesArray = lists:foldl(
        fun({Bmin, Bmax}, Ranges) ->
            {UpdatedRanges, Incorporated} = array:foldl(
                fun(I, {Amin, Amax}, {Array, Incorporated}) ->
                    if
                        Incorporated ->
                            {Array, true};
                        ((Amin =< Bmin) and (Amax >= Bmin)) or
                            ((Amin >= Bmin) and (Bmax >= Amin)) or
                            ((Amax == (Bmin - 1)) or (Bmax == (Amin - 1))) ->
                            {replace(I, {Amin, Amax}, {Bmin, Bmax}, Array), true};
                        true ->
                            {Array, false}
                    end
                end,
                {Ranges, false},
                Ranges
            ),
            case Incorporated of
                true -> UpdatedRanges;
                false -> array:set(array:size(Ranges), {Bmin, Bmax}, Ranges)
            end
        end,
        array:new(),
        RangesList
    ),
    NewRangesList = array:to_list(RangesArray),
    case length(NewRangesList) == length(RangesList) of
        true -> NewRangesList;
        false -> reduce_ranges(NewRangesList)
    end.

replace(Index, {Amin, Amax}, {Bmin, Bmax}, Array) ->
    array:set(Index, {lists:min([Amin, Bmin]), lists:max([Amax, Bmax])}, Array).

to_int(String) ->
    {Int, _} = string:to_integer(String),
    Int.

to_range(String) ->
    [Min | [Max]] = string:split(String, "-"),
    {to_int(Min), to_int(Max)}.

within(Id, {Min, Max}) ->
    (Id >= Min) and (Id =< Max).
