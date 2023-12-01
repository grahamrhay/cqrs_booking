.PHONY: compile release console

compile:
	docker run --rm -v ${PWD}:/app -w /app erlang:26 ./rebar3 compile

shell:
	docker run --rm -it -v ${PWD}:/app -w /app erlang:26 ./rebar3 shell

release:
	docker run --rm -v ${PWD}:/app -w /app erlang:26 ./rebar3 release

console:
	docker run --rm -it -v ${PWD}:/app -w /app erlang:26 _build/default/rel/cqrs_booking/bin/cqrs_booking console
