%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(error_m_collection).
-behaviour(monad).
-export(['>>='/2, return/1, fail/1]).
-export([add_state/2, replace/2, unpack/1]).

-type error() :: {error, atom()}.
-type state() :: any().
-type return(Type) :: {ok, Type} | error().
-type contents(Type) :: [
    error_m_single:monad(Type) |
    error_m_multiple:monad(Type) |
    error_m_multiple_info:monad(Type) |
    error() ].
-type monad_stateless(Type) :: {collection, contents(Type)} | error().
-type monad_state(Type)     :: {collection, contents(Type), state()} | error().
-type monad(Type)           :: monad_stateless(Type) | monad_state(Type).

-export_type([monad/1]).

-compile({parse_transform, do}).

-spec '>>='(monad(Type), fun()) -> monad(Type).
'>>='({collection, List}, Fun) ->
    {All, OK} = monads_help:unzipwith(fun(Elem) ->
        Init = do([error_m_collection_select ||
            error_m_simple:init(Elem),
            error_m_multiple:init(Elem),
            error_m_multiple_info:init(Elem)
        ]),
        case Init of
            {error, _} = Error     -> {Error, Error};
            {normal, Elem, Opaque} -> {Opaque, Elem}
        end
    end, List),
    case Fun(OK) of
        {collection, NList} ->
            NNList = lists:zipwith(fun(Ret, Opaque) ->
                do([error_m_collection_select ||
                    error_m_simple:finish(Ret, Opaque),
                    error_m_multiple:finish(Ret, Opaque),
                    error_m_multiple_info:finish(Ret, Opaque)
                ])
            end, All, NList),
            {collection, NNList};
        {error, Error} ->
            {error, Error}
    end;
'>>='({error, Error}, _Fun) ->
    {error, Error}.

-spec return(Type) -> monad(Type).
return(List) when is_list(List) -> {collection, List};
return(Elem)                    -> {collection, [Elem]}.

-spec fail(atom()) -> error().
fail(Error) -> {error, Error}.

-spec add_state(monad_stateless(Type), state()) -> monad_state(Type).
add_state({collection, List}, State) when is_list(State) ->
    {collection, List, State};
add_state({error, Error}, _State) ->
    {error, Error}.

-spec replace(monad(any()), [Type]) -> monad(Type).
replace({collection, List}, RepList) when is_list(RepList) ->
    {collection, rep(List, RepList)};
replace({collection, List, State}, RepList) when is_list(RepList) ->
    {collection, rep(List, RepList), State};
replace({error, Error}, _List) ->
    {error, Error}.

-spec unpack(monad(Type)) -> return([contents(Type)]).
unpack({collection, List}) ->
    {ok, List};
unpack({collection, List, _State}) ->
    {ok, List};
unpack({error, Error}) ->
    {error, Error}.

% Helper

rep(List, RepList) ->
    lists:zipwith(fun
        ({error, Error}, _RMonad) -> {error, Error};
        (_Monad, RMonad)          -> RMonad
    end, List, RepList).

