-module(day01).

main(_) ->
    {ok, Data} = file:read_file("day01.txt"),
    StepBits = lists:droplast(string:split(Data, "\n", all)),
    Steps = lists:map(fun bitstring_to_list/1, StepBits),
    {_, FirstPasscode} = lists:foldl(fun turn1/2, {50, 0}, Steps),
    {_, SecondPasscode} = lists:foldl(fun turn2/2, {50, 0}, Steps),
    io:format("Part One: ~p~nPart Two: ~p~n", [FirstPasscode, SecondPasscode]).

calc(Num) when Num < 0 -> calc(Num + 100);
calc(Num) -> Num rem 100.

to_number(Step) ->
    {Dir, NumString} = lists:split(1, Step),
    {Number, _} = string:to_integer(NumString),
    if
        Dir == "L" -> 0 - Number;
        true -> Number
    end.

turn1(Step, {Dial, Clicks}) ->
    NewDial = calc(Dial + to_number(Step)),
    MoreClicks =
        if
            NewDial == 0 -> 1;
            true -> 0
        end,
    {NewDial, Clicks + MoreClicks}.

turn2(Step, {Dial, Clicks}) ->
    Change = to_number(Step),
    Total = Dial + Change,
    NewDial = calc(Total),
    MoreClicks =
        if
            (Total < 0) and (Dial == 0) -> floor(abs(Total) / 100);
            Total < 0 -> 1 + floor(abs(Total) / 100);
            Total == 0 -> 1;
            Total > 0 -> floor(Total / 100)
        end,
    {NewDial, Clicks + MoreClicks}.
