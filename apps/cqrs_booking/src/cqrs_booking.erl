-module(cqrs_booking).

-export([
         start_link/0
        ]).

-behaviour(gen_server).

-export([ init/1 , handle_call/3 , handle_cast/2 , handle_info/2 ]).

-type state() :: #{}.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, unused, []).

-spec init(unused) -> {ok, state()}.
init(unused) ->
    State = #{
              available_rooms => cqrs_booking_hotels:get_available_rooms()
             },
    {ok, State}.

-spec handle_call(any(), {pid(), any()}, state()) ->
    {reply, any(), state()} | {noreply, state()}.
handle_call(_Request, _From, State) ->
    {noreply, State}.

-spec handle_cast(any(), state()) ->
    {reply, any(), state()} | {noreply, state()}.
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(_Request, State) ->
    {noreply, State}.
