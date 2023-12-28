-module(cqrs_booking).

-export([
         start_link/0
        ]).

-behaviour(gen_server).

-export([ init/1 , handle_call/3 , handle_cast/2 , handle_info/2, handle_continue/2, terminate/2 ]).

-include_lib("stdlib/include/ms_transform.hrl").

-type state() :: #{}.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, unused, []).

-spec init(unused) -> {ok, state()}.
init(unused) ->
    {ok, _} = dets:open_file(snapshot, []),
    Results = dets:lookup(snapshot, latest),
    State = case length(Results) of
        0 ->
            new_state();
        1 ->
            [{latest, S}] = Results,
            S
    end,
    {ok, _} = dets:open_file(bookings, []),
    LatestVersion = maps:get(version, State),
    io:format("Current version: ~p~n", [LatestVersion]),
    MatchSpec = ets:fun2ms(fun({N,Cmd}) when N > LatestVersion -> {N, Cmd} end),
    ExistingBookings = lists:sort(fun({A,_}, {B,_}) -> A =< B end, dets:select(bookings, MatchSpec)),
    io:format("Replaying bookings: ~p~n", [ExistingBookings]),
    NewBookings = lists:foldl(fun({_, Cmd}, B) -> add_new_booking(Cmd, B) end, maps:get(bookings, State), ExistingBookings),
    erlang:send_after(60 * 1000, self(), save_snapshot),
    {ok, State#{bookings:=NewBookings, version:=LatestVersion + length(ExistingBookings)}}.

-spec handle_call(any(), {pid(), any()}, state()) ->
    {reply, any(), state()} | {noreply, state()}.
handle_call({book_room, Cmd}, _From, #{available_rooms:=AvailableRooms, version:=Version} = State) ->
    {_Client, Hotel, Room, CheckIn, _CheckOut} = Cmd,
    AvailableRoomsForHotel = maps:get(Hotel, AvailableRooms),
    AvailableRoomsForDay = maps:get(CheckIn, AvailableRoomsForHotel),
    [_RoomInfo] = lists:filter(fun(R) -> maps:get(id, R) == Room end, AvailableRoomsForDay),
    NewVersion = Version + 1,
    ok = dets:insert(bookings, {NewVersion, Cmd}),
    {reply, ok, State#{version:=NewVersion}, {continue, {new_booking, Cmd}}};
handle_call({get_bookings, Client}, _From, #{bookings:=Bookings} = State) ->
    BookingsForClient = maps:get(Client, Bookings),
    {reply, {ok, BookingsForClient}, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

-spec handle_cast(any(), state()) ->
    {reply, any(), state()} | {noreply, state()}.
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(save_snapshot, State) ->
    io:format("Saving snapshot~n", []),
    ok = dets:insert(snapshot, {latest, State}),
    {noreply, State};
handle_info(_Request, State) ->
    {noreply, State}.

-spec handle_continue(term(), state()) -> {noreply, state()}.
handle_continue({new_booking, Cmd}, #{bookings:=Bookings} = State) ->
    NewBookings = add_new_booking(Cmd, Bookings),
    {noreply, State#{bookings:=NewBookings}};
handle_continue(_Continue, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    dets:close(bookings),
    ok.

add_new_booking({Client, _Hotel, _Room, _CheckIn, _CheckOut} = Cmd, Bookings) ->
    case maps:is_key(Client, Bookings) of
        true ->
            BookingsForClient = maps:get(Client, Bookings),
            maps:update(Client, [Cmd | BookingsForClient], Bookings);
        false ->
            maps:put(Client, [Cmd], Bookings)
    end.

new_state() ->
    #{
      available_rooms => cqrs_booking_hotels:get_available_rooms(),
      bookings => #{},
      version => 0
     }.
