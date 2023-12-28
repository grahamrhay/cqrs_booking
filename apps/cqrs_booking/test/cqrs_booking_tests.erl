-module(cqrs_booking_tests).

-include_lib("eunit/include/eunit.hrl").

init_test_() ->
    [
     init_available_rooms(),
     init_bookings_list_is_empty()
    ].

init_available_rooms() ->
    clean_dets_files(),
    {ok, #{available_rooms:=AvailableRooms}} = cqrs_booking:init(unused),
    cqrs_booking:terminate(shutdown, #{}),
    ?_assertEqual(AvailableRooms, cqrs_booking_hotels:get_available_rooms()).

init_bookings_list_is_empty() ->
    clean_dets_files(),
    {ok, #{bookings:=Bookings}} = cqrs_booking:init(unused),
    cqrs_booking:terminate(shutdown, #{}),
    ?_assertEqual(0, length(maps:keys(Bookings))).

handle_call_test_() ->
    [
     {setup, fun start/0, fun stop/1, fun new_booking_should_be_added/1},
     unavailable_room_cannot_be_booked()
    ].

restore_test_() ->
    [
     bookings_should_survive_restart()
    ].

start() ->
    clean_dets_files(),
    {ok, Pid} = cqrs_booking:start_link(),
    Pid.

clean_dets_files() ->
    lists:foreach(fun(F) ->
                          ok = case file:delete(F) of ok -> ok; {error, enoent} -> ok end
                  end, ["bookings", "snapshot"]).

stop(Pid) ->
    ok = gen_server:stop(Pid).

new_booking_should_be_added(Pid) ->
    Client = 1,
    Cmd = {Client, 2, <<"101">>, {2023, 12, 1}, {2023, 12, 2}},
    ok = gen_server:call(Pid, {book_room, Cmd}),
    {ok, BookingsForClient} = gen_server:call(Pid, {get_bookings, Client}),
    [
     ?_assertEqual(1, length(BookingsForClient)),
     ?_assertEqual([Cmd], BookingsForClient)
    ].

unavailable_room_cannot_be_booked() ->
    Cmd = {1, 2, <<"666">>, {2023, 12, 1}, {2023, 12, 2}},
    State = new_state(),
    ?_assertError({badmatch, []}, cqrs_booking:handle_call({book_room, Cmd}, self(), State)).

new_state() ->
    #{
      available_rooms => cqrs_booking_hotels:get_available_rooms(),
      bookings => #{},
      version => 0
     }.

bookings_should_survive_restart() ->
    Pid1 = start(),
    Client = 1,
    Cmd = {Client, 2, <<"101">>, {2023, 12, 1}, {2023, 12, 2}},
    ok = gen_server:call(Pid1, {book_room, Cmd}),
    stop(Pid1),
    {ok, Pid2} = cqrs_booking:start_link(),
    {ok, BookingsForClient} = gen_server:call(Pid2, {get_bookings, Client}),
    [
     ?_assertEqual(1, length(BookingsForClient))
    ].
