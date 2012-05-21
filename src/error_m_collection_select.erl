%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(error_m_collection_select).
-behaviour(monad).
-export(['>>='/2, return/1, fail/1]).

-type error()               :: {error, atom()}.
-type opaque()              :: any().
-type state()               :: any().
-type unknown(Type)         :: {unknown, monad(Type)}.
-type monad_stateless(Type) :: {normal, Type, opaque()}.
-type monad_state(Type)     :: {normal, {Type, state()}, opaque()}.
-type monad(Type)           :: monad_stateless(Type) | monad_state(Type) | error() | unknown(Type).

-export_type([monad/1]).

-spec '>>='(monad(Type), fun()) -> monad(Type).
'>>='({unknown, Monad}, Fun) -> Fun(Monad);
'>>='(Init, _Fun)            -> Init.

-spec return({Type, any()} | error()) -> monad(Type).
return({error, Error}) -> {error, Error};
return({Elem, Opaque}) -> {normal, Elem, Opaque}.

-spec fail(any()) -> unknown(any()).
fail(Monad) -> {unknown, Monad}.

