%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(error_m_simple).
-behaviour(monad).
-export(['>>='/2, return/1, fail/1, init/1, finish/2]).
-export([add_state/2, merge_state/1, replace/2, unpack/1, merge/2, merge/3]).
-export([upgrade/1, upgrade_info/1]).

-type error()               :: {error, atom()}.
-type return(Type)          :: {ok, Type} | error().
-type state()               :: any().
-type monad_stateless(Type) :: {ok, Type} | error().
-type monad_state(Type)     :: {ok, Type, state()} | error().
-type monad(Type)           :: monad_stateless(Type) | monad_state(Type).

-export_type([monad/1]).

-spec '>>='(monad(Type), fun()) -> monad(Type).
'>>='(Monad, Fun) ->
    case init(Monad) of
        {normal, Elem, null} -> finish(Fun(Elem), null);
        {error, Error}       -> {error, Error}
    end.

-spec return(Type) -> monad(Type).
return({ok, Elem})     -> {ok, Elem};
return({error, Error}) -> {error, Error};
return(Elem)           -> {ok, Elem}.

-spec fail(atom()) -> error().
fail(Error) -> {error, Error}.

-spec init(monad(Type)) -> error_m_collection_select:monad(Type).
init({ok, Elem})        -> {normal, Elem, null};
init({ok, Elem, State}) -> {normal, {Elem, State}, null};
init({error, Error})    -> {error, Error};
init({return, Elem})    -> init(return(Elem));
init(Monad)             -> {unknown, Monad}.

-spec finish(monad(Type), any()) -> monad(Type).
finish({ok, OK}, _)       -> {ok, OK};
finish({error, Error}, _) -> {error, Error};
finish(Monad, _)          -> {unknown, Monad}.

-spec add_state(monad_stateless(Type), state()) -> monad_state(Type).
add_state({ok, Elem}, State) ->
    {ok, Elem, State};
add_state({error, Error}, _State) ->
    {error, Error}.

-spec merge_state(monad_state(Type)) -> monad_stateless({Type, state()}).
merge_state({ok, Elem, State}) ->
    {ok, {Elem, State}};
merge_state({error, Error}) ->
    {error, Error}.

-spec replace(monad(any()), Type) -> monad(Type).
replace({ok, _Elem}, Rep) ->
    {ok, Rep};
replace({ok, _Elem, State}, Rep) ->
    {ok, Rep, State};
replace({error, Error}, _Rep) ->
    {error, Error}.

-spec unpack(monad(Type)) -> return(Type).
unpack({ok, Elem}) ->
    {ok, Elem};
unpack({ok, Elem, _State}) ->
    {ok, Elem};
unpack({error, Error}) ->
    {error, Error}.

-spec merge(monad(Type), monad(Type)) -> monad(Type).
merge({error, Error}, _) -> {error, Error};
merge(_, {error, Error}) -> {error, Error};
merge(_, {ok, OK})       -> {ok, OK}.

-spec merge(monad(Type), monad(Type), monad(Type)) -> monad(Type).
merge({error, Error}, _, _) -> {error, Error};
merge(_, {error, Error}, _) -> {error, Error};
merge(_, _, {error, Error}) -> {error, Error};
merge(_, _, {ok, OK})       -> {ok, OK}.

-spec upgrade(monad(Type)) -> error_m_multiple:monad(Type).
upgrade({ok, Elem})     -> error_m_multiple:return(Elem);
upgrade({error, Error}) -> {error, Error}.

-spec upgrade_info(monad(Type)) -> error_m_multiple_info:monad(Type).
upgrade_info({ok, Elem})     -> error_m_multiple_info:return(Elem);
upgrade_info({error, Error}) -> {error, Error}.

