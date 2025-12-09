.PHONY: all compile xref eunit ct dialyzer doc callgraph graphviz clean distclean

REBAR := rebar3
PLT_FILE = .leo_watchdog_dialyzer_plt
DOT_FILE = leo_watchdog.dot
CALL_GRAPH_FILE = leo_watchdog.png

all: compile xref eunit

compile:
	@$(REBAR) compile

xref:
	@$(REBAR) xref

eunit:
	@$(REBAR) eunit

ct:
	@$(REBAR) ct

dialyzer:
	@$(REBAR) dialyzer

typer:
	typer --plt $(PLT_FILE) -I include/ -r src/

doc:
	@$(REBAR) edoc

callgraph: graphviz
	dot -Tpng -o$(CALL_GRAPH_FILE) $(DOT_FILE)

graphviz:
	$(if $(shell which dot),,$(error "To make the depgraph, you need graphviz installed"))

clean:
	@$(REBAR) clean

distclean:
	@rm -rf _build
	@$(REBAR) clean
