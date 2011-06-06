all: clean build package

clean:
	rebar clean

deps:
	rebar get-deps

build: deps
	rebar compile

package: build
	rebar generate

console: package
	rel/{{projectid}}/bin/{{projectid}} console

devconsole: package
	erl -pa deps/*/ebin -pa apps/*/ebin -boot start_sasl -s {{projectid}}

dialyze: clean build-plt
	rebar compile debug_info=1
	rebar dialyze skip_deps=true
	rebar clean

build-plt:
	rebar build-plt skip_deps=true

