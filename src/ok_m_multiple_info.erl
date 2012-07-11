%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(ok_m_multiple_info).
-behaviour(monad).
-export(['>>='/2, return/1, fail/1, init/1, finish/2, add_state/2]).

-type error()               :: {error, atom()}.
-type return(Type)          :: {ok, Type} | error().
-type state()               :: [any()] | {[any()], [any()]}.
-type monad_stateless(Type) :: {ok_multiple_info, [return(Type)], {integer(), integer()}} | error().
-type monad_state(Type)     :: {ok_multiple_info, [return(Type)], {integer(), integer()}, state()} | error().
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
    {ok_multiple_info, [{ok, Elem}], {1, 1}};
return({error, Error}) ->
    {error, Error};
return({multiple, List}) ->
    return(List);
return({multiple_info, List, {Count, Failed}}) ->
    {ok_multiple_info, List, {Count, Count-Failed}};
return(List) when is_list(List) ->
    {NList, Info} = lists:mapfoldl(fun
        ({ok, Elem}, {Count, OK})     -> {{ok, Elem}, {Count+1, OK+1}};
        ({error, Error}, {Count, OK}) -> {{error, Error}, {Count+1, OK}};
        (Elem, {Count, OK})           -> {{ok, Elem}, {Count+1, OK+1}}
    end, {0, 0}, List),
    {ok_multiple_info, NList, Info};
return(Elem) ->
    {ok_multiple_info, [{ok, Elem}], {1, 1}}.

-spec fail(atom()) -> error().
fail(Error) ->
    {error, Error}.

-spec init(monad(Type)) -> error_m_collection_select:monad([Type]).
init({ok_multiple_info, List, Info}) ->
    {All, Failed} = split(List),
    {normal, Failed, {All, Info}};
init({ok_multiple_info, List, Info, State}) ->
    {All, Failed, NState} = split(List, State),
    {normal, {Failed, NState}, {All, Info}};
init({error, Error}) ->
    {error, Error};
init({return, Elem}) ->
    init(return(Elem));
init(Monad) ->
    {unknown, Monad}.

-spec finish(monad(Type), any()) -> monad(Type).
finish({ok_multiple_info, List, {_Count, NOK}}, {All, {Count, OK}}) ->
    {NList, Excess} = join(All, List),
    {ok_multiple_info, NList, {Count-Excess, OK+NOK}};
finish({multiple_info, List, {NCount, Failed}}, {All, {Count, OK}}) ->
    {NList, Excess} = join(All, List),
    {ok_multiple_info, NList, {Count-Excess, OK+NCount-Failed}};
finish({error, Error}, _) ->
    {error, Error};
finish(Monad, _) ->
    {unknown, Monad}.

-spec add_state(monad_stateless(Type), state()) -> monad_state(Type).
add_state({ok_multiple_info, List, Info}, State) ->
    {ok_multiple_info, List, Info, State};
add_state({error, Error}, _State) ->
    {error, Error}.


% Helper

split(XObjs) ->
    lists:foldr(fun
        ({ok, Ret}, {All, Failed})      -> {[{ok, Ret}|All], Failed};
        ({error, Error}, {All, Failed}) -> {[fail|All], [Error|Failed]}
    end, {[], []}, XObjs).
split(XObjs, State) ->
    monads_help:foldr2(fun
        ({ok, Ret}, _S, {All, Failed, NState})     -> {[{ok, Ret}|All], Failed, NState};
        ({error, Error}, S, {All, Failed, NState}) -> {[fail|All], [Error|Failed], [S|NState]}
    end, {[], [], []}, XObjs, State).

join(All, New) ->
    {[], Acc, Excess} = lists:foldr(fun
        (fail, {[N|NL], Acc, Excess})  -> {NL, [N|Acc], Excess};
        (fail, {[], Acc, Excess})      -> {[], Acc, Excess+1};
        ({ok, Ret}, {NL, Acc, Excess}) -> {NL, [{ok, Ret}|Acc], Excess}
    end, {lists:reverse(New), [], 0}, All),
    {Acc, Excess}.

