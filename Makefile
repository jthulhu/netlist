TARGETS = netlist_simulator scheduler_test graph_tes

all: out/netlist_simulator

build: src
	dune build

out/%.exe: src/
	dune build $(patsubst out/%.exe,src/%.exe,$@)

out/%: out/%.exe
	@cp $< $@
clean:
	dune clean
	$(RM) out/*

tests:
	./run-tests

.PHONY: all clean install build $(patsubst %,out/%.exe,$TARGETS) tests
