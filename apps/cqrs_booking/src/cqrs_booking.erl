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
    Version = 0,
    State = #{
              available_rooms => cqrs_booking_hotels:get_available_rooms(),
              bookings => #{},
              version => Version
             },
    {ok, _Name} = dets:open_file(bookings, []),
    MatchSpec = ets:fun2ms(fun({N,Cmd}) when N >= Version -> {N, Cmd} end),
    ExistingBookings = lists:sort(fun({A,_}, {B,_}) -> A =< B end, dets:select(bookings, MatchSpec)),
    io:format("Existing bookings: ~p~n", [ExistingBookings]),
    NewBookings = lists:foldl(fun({_, Cmd}, B) -> add_new_booking(Cmd, B) end, maps:get(bookings, State), ExistingBookings),
    {ok, State#{bookings:=NewBookings, version:=Version + length(ExistingBookings)}}.

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
