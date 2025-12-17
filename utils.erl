-module(utils).
-export([read_lines/1]).

%{ok, File} = file:open("day01_demo.txt", read),
%io:format(io:get_line(File, '')).

read_lines(FileName) ->
    {ok, Data} = file:read_file("day01.txt"),
    DataBits = lists:droplast(string:split(Data, "\n", all)),
    Data = lists:map(fun bitstring_to_list/1, DataBits).
