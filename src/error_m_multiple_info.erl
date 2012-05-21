%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(error_m_multiple_info).
-behaviour(monad).
-export(['>>='/2, return/1, fail/1, init/1, finish/2]).
-export([add_state/2, add_state/3, merge_state/1, replace/2, unpack/1, merge/2, merge/3]).
-export([downgrade/1, first_error/1, info_count/1, info_fail/1]).

-type error()               :: {error, atom()}.
-type return(Type)          :: {ok, Type} | error().
-type state()               :: [any()] | {[any()], [any()]}.
-type monad_stateless(Type) :: {ok, multiple_info, [return(Type)], {integer(), integer()}} | error().
-type monad_state(Type)     :: {ok, multiple_info, [return(Type)], {integer(), integer()}, state()} | error().
-type monad(Type)           :: monad_stateless(Type) | monad_state(Type).

-export_type([monad/1]).

-spec '>>='(monad(Type), fun()) -> monad(Type).
'>>='(Monad, Fun) ->
    case init(Monad) of
        {normal, Elem, Opaque} -> finish(Fun(Elem), Opaque);
        {error, Error}         -> {error, Error}
    end.

-spec return([return(Type)] | return(Type) | [Type]) -> monad(Type).
return({ok, Elem}) ->
    {multiple_info, [{ok, Elem}], {1, 0}};
return({error, Error}) ->
    {error, Error};
return(List) when is_list(List) ->
    {NList, Info} = lists:mapfoldl(fun
        ({ok, Elem}, {Count, Fail})     -> {{ok, Elem}, {Count+1, Fail}};
        ({error, Error}, {Count, Fail}) -> {{error, Error}, {Count+1, Fail+1}};
        (Elem, {Count, Fail})           -> {{ok, Elem}, {Count+1, Fail}}
    end, {0, 0}, List),
    {multiple_info, NList, Info};
return(Elem) ->
    {multiple_info, [{ok, Elem}], {1, 0}}.

-spec fail(atom()) -> error().
fail(Error) ->
    {error, Error}.

-spec init(monad(Type)) -> error_m_collection_select:monad([Type]).
init({multiple_info, List, Info}) ->
    {All, OK} = split(List),
    {normal, OK, {All, Info}};
init({multiple_info, List, Info, State}) ->
    {All, OK, NState} = split(List, State),
    {normal, {OK, NState}, {All, Info}};
init({error, Error}) ->
    {error, Error};
init(Monad) ->
    {unknown, Monad}.

-spec finish(monad(Type), any()) -> monad(Type).
finish({multiple_info, List, {_Count, NFail}}, {All, {Count, Fail}}) ->
    {multiple_info, join(All, List), {Count, Fail+NFail}};
finish({error, Error}, _) ->
    {error, Error};
finish(Monad, _) ->
    {unknown, Monad}.

-spec add_state(monad_stateless(Type), state()) -> monad_state(Type).
add_state({multiple_info, List, Info}, State) ->
    {multiple_info, List, Info, State};
add_state({error, Error}, _State) ->
    {error, Error}.

-spec add_state(monad_stateless(Type), state(), state()) -> monad_state(Type).
add_state({multiple_info, List, Info}, State1, State2) ->
    {multiple_info, List, Info, {State1, State2}};
add_state({error, Error}, _State1, _State2) ->
    {error, Error}.

-spec merge_state(monad_state(Type)) -> monad_stateless({Type, any()}).
merge_state({multiple_info, List, Info, State}) ->
    {multiple_info, {List, State}, Info};
merge_state({error, Error}) ->
    {error, Error}.

-spec replace(monad(any())|error_m_multiple:monad(any()), [Type]) -> monad(Type).
replace({multiple_info, List, Info}, RepList) when is_list(RepList) ->
    {multiple_info, rep(List, RepList), Info};
replace({multiple_info, List, Info, State}, RepList) when is_list(RepList) ->
    {multiple_info, rep(List, RepList), Info, State};
replace({multiple, List}, RepList) when is_list(RepList) ->
    {NList, Info} = rep_count(List, RepList),
    {multiple_info, NList, Info};
replace({multiple, List, State}, RepList) when is_list(RepList) ->
    {NList, Info} = rep_count(List, RepList),
    {multiple_info, NList, Info, State};
replace({error, Error}, _List) ->
    {error, Error}.

-spec unpack(monad(Type)) -> return([return(Type)]).
unpack({multiple_info, List, _Info}) ->
    {ok, List};
unpack({multiple_info, List, _Info, _State}) ->
    {ok, List};
unpack({error, Error}) ->
    {error, Error}.

-spec merge(monad(Type), monad(Type)) -> monad(Type).
merge({error, Error}, _) -> {error, Error};
merge(_, {error, Error}) -> {error, Error};
merge({multiple_info, List1, {Count, _}}, {multiple_info, List2, {Count, _}}) ->
    {List, Fail} = monads_help:zipwithfold(fun
        ({error, Error}, _, Fail) -> {{error, Error}, Fail+1};
        (_, {error, Error}, Fail) -> {{error, Error}, Fail+1};
        (_, {ok, OK}, Fail)       -> {{ok, OK}, Fail}
    end, 0, List1, List2),
    {multiple_info, List, {Count, Fail}}.

-spec merge(monad(Type), monad(Type), monad(Type)) -> monad(Type).
merge({error, Error}, _, _) -> {error, Error};
merge(_, {error, Error}, _) -> {error, Error};
merge(_, _, {error, Error}) -> {error, Error};
merge({multiple_info, List1, {Count, _}}, {multiple_info, List2, {Count, _}},
      {multiple_info, List3, {Count, _}}) ->
    {List, Fail} = monads_help:zipwithfold3(fun
        ({error, Error}, _, _, Fail) -> {{error, Error}, Fail+1};
        (_, {error, Error}, _, Fail) -> {{error, Error}, Fail+1};
        (_, _, {error, Error}, Fail) -> {{error, Error}, Fail+1};
        (_, _, {ok, OK}, Fail)       -> {{ok, OK}, Fail}
    end, 0, List1, List2, List3),
    {multiple_info, List, {Count, Fail}}.

-spec downgrade(monad(Type)) -> error_m_simple:monad(Type).
downgrade({multiple_info, [Elem|_], _Info}) ->
    error_m_simple:return(Elem);
downgrade({error, Error}) ->
    {error, Error}.

-spec info_count(monad(any())) -> integer().
info_count({multiple_info, _List, {Count, _Fail}}) -> Count.

-spec info_fail(monad(any())) -> integer().
info_fail({multiple_info, _List, {_Count, Fail}}) -> Fail.

-spec first_error(monad(any())) -> error().
first_error({multiple_info, [{error, Error}|_List], _Info}) ->
    {error, Error};
first_error({multiple_info, [{ok, _}|List], _Info}) ->
    first_error({multiple_info, List, null}).


% Helper

split(XObjs) ->
    lists:foldr(fun
        ({ok, Obj}, {All, OK})      -> {[ok|All], [Obj|OK]};
        ({error, Error}, {All, OK}) -> {[{error, Error}|All], OK}
    end, {[], []}, XObjs).
split(XObjs, {State1, State2}) ->
    monads_help:foldr3(fun
        ({ok, Obj}, S1, S2, {All, OK, NState1, NState2}) ->
            {[ok|All], [Obj|OK], [S1|NState1], [S2|NState2]};
        ({error, Error}, _S1, _S2, {All, OK, NState1, NState2}) ->
            {[{error, Error}|All], OK, NState1, NState2}
    end, {[], [], [], []}, XObjs, State1, State2);
split(XObjs, State) ->
    monads_help:foldr2(fun
        ({ok, Obj}, S, {All, OK, NState})       -> {[ok|All], [Obj|OK], [S|NState]};
        ({error, Error}, _S, {All, OK, NState}) -> {[{error, Error}|All], OK, NState}
    end, {[], [], []}, XObjs, State).

join(All, New) ->
    {[], Acc} = lists:foldr(fun
        (ok, {[N|NL], Acc})         -> {NL, [N|Acc]};
        ({error, Error}, {NL, Acc}) -> {NL, [{error, Error}|Acc]}
    end, {lists:reverse(New), []}, All),
    Acc.

rep(List, RepList) ->
    lists:zipwith(fun
        ({ok, _Data}, Elem)     -> {ok, Elem};
        ({error, Error}, _Elem) -> {error, Error}
    end, List, RepList).

rep_count(List, RepList) ->
    monads_help:foldr2(fun
        ({ok, _Data}, Elem, {Akk, {C, F}})     -> {[{ok, Elem}|Akk], {C+1, F}};
        ({error, Error}, _Elem, {Akk, {C, F}}) -> {[{error, Error}|Akk], {C+1, F+1}}
    end, {[], {0, 0}}, List, RepList).

