CQRS Booking
=====

https://codingdojo.org/kata/CQRS_Booking/

Build
-----

    $ make compile

Run
-----

    $ make shell

Add booking
-----

    1> gen_server:call(cqrs_booking, {book_room, {1, 2, <<"101">>, {2023, 12, 1}, {2023, 12, 2}}}).

Re-compile
-----

    2> r3:compile().
