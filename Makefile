PROJECT=swirl
REBAR=./rebar

all: deps compile doc

build-plt: all
	@dialyzer --build_plt --output_plt ~/.$(PROJECT).plt \
		--apps erts kernel stdlib crypto public_key ssl

clean:
	@$(REBAR) clean
	@rm -rf deps ebin doc/edoc-info doc/*.md README.md

compile:
	@echo "Running rebar compile..."
	@$(REBAR) compile

deps:
	@echo "Running rebar get-deps..."
	@$(REBAR) update-deps

dialyze:
	@dialyzer ebin/*.beam --plt ~/.$(PROJECT).plt | \
	fgrep -v -f ./priv/dialyzer.ignore-warnings

doc:
	@echo "Running rebar doc..."
	@$(REBAR) skip_deps=true doc

eunit:
	@echo "Running EUnit suite..."
	@$(REBAR) skip_deps=true eunit

test: all eunit

.PHONY: deps doc
