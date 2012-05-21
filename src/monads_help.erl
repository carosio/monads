%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(monads_help).

-export([unzipwith/2, zipwithfold/4, zipwithfold3/5, foldr2/4, foldr3/5]).

unzipwith(Fun, List) ->
    lists:foldr(fun(Elem, {AL, BL}) ->
        {A, B} = Fun(Elem),
        {[A|AL], [B|BL]}
    end, {[], []}, List).


zipwithfold(Fun, Acc, List1, List2) ->
    zipwithfoldb(Fun, List1, List2, [], Acc).
zipwithfoldb(_Fun, [], [], Acc1, Acc2) ->
    {lists:reverse(Acc1), Acc2};
zipwithfoldb(Fun, [L1|List1], [L2|List2], Acc1, Acc2) ->
    {NAcc1, NAcc2} = Fun(L1, L2, Acc2),
    zipwithfoldb(Fun, List1, List2, [NAcc1|Acc1], NAcc2).


zipwithfold3(Fun, Acc, List1, List2, List3) ->
    zipwithfold3b(Fun, List1, List2, List3, [], Acc).
zipwithfold3b(_Fun, [], [], [], Acc1, Acc2) ->
    {lists:reverse(Acc1), Acc2};
zipwithfold3b(Fun, [L1|List1], [L2|List2], [L3|List3], Acc1, Acc2) ->
    {NAcc1, NAcc2} = Fun(L1, L2, L3, Acc2),
    zipwithfold3b(Fun, List1, List2, List3, [NAcc1|Acc1], NAcc2).


foldl2(Fun, Akk, [L1|List1], [L2|List2]) ->
    foldl2(Fun, Fun(L1, L2, Akk), List1, List2).

foldr2(Fun, Akk, List1, List2) ->
    foldl2(Fun, Akk, lists:reverse(List1), lists:reverse(List2)).

foldl3(_Fun, Akk, [], [], []) -> Akk;
foldl3(Fun, Akk, [L1|List1], [L2|List2], [L3|List3]) ->
    foldl3(Fun, Fun(L1, L2, L3, Akk), List1, List2, List3).

foldr3(Fun, Akk, List1, List2, List3) ->
    foldl3(Fun, Akk, lists:reverse(List1), lists:reverse(List2), lists:reverse(List3)).

