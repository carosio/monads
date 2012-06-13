%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(task_m).
-behaviour(monad).
-export(['>>='/2, return/1, fail/1, exec/2]).

-type error()      :: {error, atom()}.
-type return(Type) :: {ok, Type} | error().
-type task()       :: {task, any()}.
-type monad(Type)  :: {tasks, [task() | return(Type)]} | {ok, [return(Type)]} | error().

-export_type([monad/1]).

-compile({parse_transform, do}).

-spec '>>='(monad(Type), fun()) -> monad(Type).
'>>='({tasks, List}, Fun) ->
    case part(List) of
        {All, []} ->
            {ok, All};
        {All, Todo} ->
            case Fun(Todo) of
                {ok, Ret}      -> {ok, merge(All, Ret)};
                {error, Error} -> {error, Error}
            end
    end;
'>>='({error, Error}, _Fun) ->
    {error, Error};
'>>='({return, Elem}, Fun) ->
    '>>='(return(Elem), Fun).

-spec return([return(Type)] | return(Type) | [Type]) -> monad(Type).
return([{task, _T}|_] = List) ->
    {tasks, List};
return({task, T}) ->
    {tasks, [{task, T}]};
return({error, Error}) ->
    {error, Error};
return(List) when is_list(List) ->
    {tasks, lists:map(fun(T) -> {task, T} end, List)};
return(T) ->
    {tasks, [{task, T}]}.

-spec fail(atom()) -> error().
fail(Error) ->
    {error, Error}.

-spec exec(monad(Type), fun()) -> {ok, [return(Type)]} | error().
exec(Tasks, Fun) ->
    do([task_m ||
        List <- Tasks,
        exec(Fun(List), Fun)
    ]).


% Helper

part(XObjs) ->
    lists:foldr(fun
        ({ok, Obj}, {All, Todo})          -> {[{ok, Obj}|All], Todo};
        ({error, Error}, {All, Todo})     -> {[{error, Error}|All], Todo};
        ({error, Code, Msg}, {All, Todo}) -> {[{error, Code, Msg}|All], Todo};
        ({task, Task}, {All, Todo})       -> {[todo|All], [{task, Task}|Todo]}
    end, {[], []}, XObjs).

merge(All, New) ->
    {[], Acc} = lists:foldr(fun
        (todo, {[N|NL], Acc})           -> {NL, [N|Acc]};
        ({ok, Obj}, {NL, Acc})          -> {NL, [{ok, Obj}|Acc]};
        ({error, Error}, {NL, Acc})     -> {NL, [{error, Error}|Acc]};
        ({error, Code, Msg}, {NL, Acc}) -> {NL, [{error, Code, Msg}|Acc]}
    end, {lists:reverse(New), []}, All),
    Acc.

