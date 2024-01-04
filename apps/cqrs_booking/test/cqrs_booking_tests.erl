-module(cqrs_booking_tests).

-include_lib("eunit/include/eunit.hrl").

init_test_() ->
    [
     init_available_rooms(),
     init_bookings_list_is_empty()
    ].

init_available_rooms() ->
    {ok, #{available_rooms:=AvailableRooms}} = cqrs_booking:init(unused),
    ?_assertEqual(AvailableRooms, cqrs_booking_hotels:get_available_rooms()).

init_bookings_list_is_empty() ->
    {ok, #{bookings:=Bookings}} = cqrs_booking:init(unused),
    ?_assertEqual(0, length(maps:keys(Bookings))).

handle_call_test_() ->
    book_room() ++ get_bookings().

book_room() ->
    [
     new_booking_should_be_added(),
     unavailable_room_cannot_be_booked()
    ].

new_booking_should_be_added() ->
    Client = 1,
    Cmd = {Client, 2, <<"101">>, {2023, 12, 1}, {2023, 12, 2}},
    State = new_state(),
    {reply, ok, #{bookings:=Bookings}} = cqrs_booking:handle_call({book_room, Cmd}, self(), State),
    BookingsForClient = maps:get(Client, Bookings),
    [
     ?_assertEqual(1, length(maps:keys(Bookings))),
     ?_assertEqual(1, length(BookingsForClient)),
     ?_assertEqual([Cmd], BookingsForClient)
    ].

unavailable_room_cannot_be_booked() ->
    Cmd = {1, 2, <<"666">>, {2023, 12, 1}, {2023, 12, 2}},
    State = new_state(),
    ?_assertError({badmatch, []}, cqrs_booking:handle_call({book_room, Cmd}, self(), State)).

get_bookings() ->
    [
     return_all_bookings_for_client()
    ].

return_all_bookings_for_client() ->
    Client = 1,
    Cmd = {Client, 2, <<"101">>, {2023, 12, 1}, {2023, 12, 2}},
    State1 = new_state(),
    #{bookings:=Bookings} = State1,
    NewBookings = maps:put(Client, [Cmd], Bookings),
    State2 = State1#{bookings:=NewBookings},

    {reply, {ok, BookingsForClient}, State2} = cqrs_booking:handle_call({get_bookings, Client}, self(), State2),

    [
     ?_assertEqual(1, length(BookingsForClient)),
     ?_assertEqual([Cmd], BookingsForClient)
    ].

new_state() ->
    #{
      available_rooms => cqrs_booking_hotels:get_available_rooms(),
      bookings => #{}
     }.
