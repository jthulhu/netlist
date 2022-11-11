TARGETS = netlist_simulator scheduler_test graph_tes

all: out/netlist_simulator

build: out/netlist_simulator out/graph_test out/scheduler_test

out/%.exe: src/
	dune build $(patsubst out/%.exe,src/%.exe,$@)

out/%: out/%.exe
	@cp $< $@

out/%.pdf out/%.aux out/%.log &: docs/%.tex
	lualatex -output-directory=out $<

doc: out/netlist.pdf

clean:
	dune clean
	$(RM) out/*

tests:
	./run-tests

.PHONY: all clean install build $(patsubst %,out/%.exe,$TARGETS) tests doc
