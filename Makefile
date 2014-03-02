PROJECT=swirl
REBAR=./rebar

.PHONY: deps

all: deps compile

build-plt: all
	@dialyzer --build_plt --output_plt ~/.$(PROJECT).plt \
		--apps erts kernel stdlib crypto public_key ssl

check-plt:
	@dialyzer --check_plt --plt ~/.$(PROJECT).plt

clean:
	@$(REBAR) clean
	@rm -rf deps ebin

compile:
	@echo "Running rebar compile..."
	@$(REBAR) compile

deps:
	@echo "Running rebar get-deps..."
	@$(REBAR) update-deps

dialyze:
	@dialyzer ebin/*.beam --plt ~/.$(PROJECT).plt

eunit:
	@echo "Running EUnit suite..."
	@$(REBAR) skip_deps=true eunit

etest:
	@echo "Running ETest suite..."
	@ERL_LIBS=deps erlc -pa ebin -o test test/*.erl
	@priv/etest-runner

test: all eunit etest
