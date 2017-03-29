PROJECT=swirl
REBAR=./rebar3

all: compile

build-plt: all
	@dialyzer --build_plt --output_plt ~/.$(PROJECT).plt \
		--apps erts kernel stdlib crypto public_key ssl

benchmarks:
	@echo "Running benchmarks..."
	@erl -pa ebin deps/*/ebin test -noshell \
		-eval 'swirl_benchmarks:all(), init:stop().'

clean:
	@$(REBAR) clean
	@rm -rf deps ebin

compile:
	@echo "Running rebar compile..."
	@$(REBAR) compile

dialyze:
	@dialyzer ebin/*.beam --plt ~/.$(PROJECT).plt | \
	fgrep -v -f ./priv/dialyzer.ignore-warnings

eunit:
	@echo "Running EUnit suite..."
	@$(REBAR) eunit

test: build-plt dialyze eunit xref

xref:
	@$(REBAR) xref

.PHONY: benchmarks dialyze eunit xref
