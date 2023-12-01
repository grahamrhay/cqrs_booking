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
              available_rooms => #{
                                   1 => #{
                                          {2023, 12, 1} => []
                                         },
                                   2 => #{
                                          {2023, 12, 1} => [
                                                            #{
                                                              id => <<"101">>,
                                                              prices => #{
                                                                          "EUR" => #{
                                                                                     1 => 109,
                                                                                     2 => 140
                                                                                    }
                                                                         }
                                                             }
                                                           ]
                                         },
                                   3 => #{
                                          {2023, 12, 1} => [
                                                            #{
                                                              id => <<"101">>,
                                                              prices => #{
                                                                          "EUR" => #{
                                                                                     1 => 109,
                                                                                     2 => 140
                                                                                    }
                                                                         }
                                                             },
                                                            #{
                                                              id => <<"102">>,
                                                              prices => #{
                                                                          "EUR" => #{
                                                                                     1 => 109,
                                                                                     2 => 140
                                                                                    }
                                                                         }
                                                             },
                                                            #{
                                                              id => <<"201">>,
                                                              prices => #{
                                                                          "EUR" => #{
                                                                                     1 => 209,
                                                                                     2 => 240
                                                                                    }
                                                                         }
                                                             },
                                                            #{
                                                              id => <<"301">>,
                                                              prices => #{
                                                                          "EUR" => #{
                                                                                     1 => 109,
                                                                                     2 => 140
                                                                                    }
                                                                         }
                                                             },
                                                            #{
                                                              id => <<"302">>,
                                                              prices => #{
                                                                          "EUR" => #{
                                                                                     1 => 109,
                                                                                     2 => 140
                                                                                    }
                                                                         }
                                                             }
                                                           ]
                                         }
                                  }
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
