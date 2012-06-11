%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(error_m_multiple).
-behaviour(monad).
-export(['>>='/2, return/1, fail/1, init/1, finish/2]).
-export([add_state/2, merge_state/1, replace/2, unpack/1]).
-export([downgrade/1, first_error/1]).

-type error()               :: {error, atom()}.
-type return(Type)          :: {ok, Type} | error().
-type state()               :: any().
-type monad_stateless(Type) :: {multiple, [return(Type)]} | error().
-type monad_state(Type)     :: {multiple, [return(Type)], state()} | error().
-type monad(Type)           :: monad_stateless(Type) | monad_state(Type).

-export_type([monad/1]).

-spec '>>='(monad(Type), fun()) -> monad(Type).
'>>='(Monad, Fun) ->
    case init(Monad) of
        {normal, Elem, Opaque} -> finish(Fun(Elem), Opaque);
        {error, Error}         -> {error, Error}
    end.

-spec return([return(Type)] | return(Type) | [Type]) -> monad(Type).
return([{ok, _D}|_] = List) ->
    {multiple, List};
return([{error, _D}|_] = List) ->
    {multiple, List};
return({ok, Elem}) ->
    {multiple, [{ok, Elem}]};
return({error, Error}) ->
    {error, Error};
return(List) when is_list(List) ->
    {multiple, lists:map(fun(L) -> {ok, L} end, List)};
return(Elem) ->
    {multiple, [{ok, Elem}]}.

-spec fail(atom()) -> error().
fail(Error) ->
    {error, Error}.

-spec init(monad(Type)) -> error_m_collection_select:monad([Type]).
init({multiple, List}) ->
    {All, OK} = part(List),
    {normal, OK, All};
init({multiple, List, State}) ->
    {All, OK, NState} = part(List, State),
    {normal, {OK, NState}, All};
init({error, Error}) ->
    {error, Error};
init({return, Elem}) ->
    init(return(Elem));
init(Monad) ->
    {unknown, Monad}.

-spec finish(monad(Type), any()) -> monad(Type).
finish({multiple, List}, All) -> {multiple, merge(All, List)};
finish({error, Error}, _)     -> {error, Error};
finish(Monad, _)              -> {unknown, Monad}.

-spec add_state(monad_stateless(Type), state()) -> monad_state(Type).
add_state({multiple, List}, State) ->
    {multiple, List, State};
add_state({error, Error}, _State) ->
    {error, Error}.

-spec merge_state(monad_state(Type)) -> monad_stateless({Type, any()}).
merge_state({multiple, List, State}) ->
    {multiple, {List, State}};
merge_state({error, Error}) ->
    {error, Error}.

-spec replace(monad(any()), [Type]) -> monad(Type).
replace({multiple, List}, RepList) when is_list(RepList) ->
    {multiple, rep(List, RepList)};
replace({multiple, List, State}, RepList) when is_list(RepList) ->
    {multiple, rep(List, RepList), State};
replace({error, Error}, _List) ->
    {error, Error}.

-spec unpack(monad(Type)) -> return([return(Type)]).
unpack({multiple, List}) ->
    {ok, List};
unpack({multiple, List, _State}) ->
    {ok, List};
unpack({error, Error}) ->
    {error, Error}.

-spec downgrade(monad(Type)) -> error_m_simple:monad(Type).
downgrade({multiple, [Elem|_]}) ->
    error_m_simple:return(Elem);
downgrade({error, Error}) ->
    {error, Error}.

-spec first_error(monad(any())) -> error().
first_error({multiple, [{error, Error}|_List]}) ->
    {error, Error};
first_error({multiple, [{ok, _}|List]}) ->
    first_error({multiple, List}).


% Helper

part(XObjs) ->
    lists:foldr(fun
        ({ok, Obj}, {All, OK})      -> {[ok|All], [Obj|OK]};
        ({error, Error}, {All, OK}) -> {[{error, Error}|All], OK}
    end, {[], []}, XObjs).
part(XObjs, State) ->
    monads_help:foldr2(fun
        ({ok, Obj}, S, {All, OK, NState})       -> {[ok|All], [Obj|OK], [S|NState]};
        ({error, Error}, _S, {All, OK, NState}) -> {[{error, Error}|All], OK, NState}
    end, {[], [], []}, XObjs, State).

merge(All, New) ->
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

