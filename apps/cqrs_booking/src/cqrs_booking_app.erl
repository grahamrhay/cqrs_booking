%%%-------------------------------------------------------------------
%% @doc cqrs_booking public API
%% @end
%%%-------------------------------------------------------------------

-module(cqrs_booking_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    cqrs_booking_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
