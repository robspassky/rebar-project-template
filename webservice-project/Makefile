all: clean build package

clean:
	rebar clean

deps:
	rebar get-deps

build: deps
	rebar compile

package: build
	rebar generate -f

console: clean package
	rel/{{projectid}}/bin/{{projectid}} console

console_clean: clean package
	rel/{{projectid}}/bin/{{projectid}} console_clean

dialyze: clean build-plt
	rebar compile debug_info=1
	rebar dialyze skip_deps=true
	rebar clean

build-plt:
	rebar build-plt skip_deps=true

