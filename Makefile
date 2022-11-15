TARGETS = netlist_simulator scheduler_test graph_tes
SOURCES = src/ docs/ tests/ Makefile dune-project flake.nix run-tests
USEFUL_ARTIFACTS = flake.lock .envrc

out/%.exe: src/
	@mkdir -p out
	dune build $(patsubst out/%.exe,src/%.exe,$@)

out/%: out/%.exe
	@cp $< $@

out/%.pdf out/%.aux out/%.log &: docs/%.tex
	@mkdir -p out
	lualatex -output-directory=out $<

out/mathieu.tgz: $(SOURCES) $(USEFUL_ARTIFACTS)
	@mkdir -p out
	tar -czvf $@ $^ --transform 's,^,mathieu/,'

archive: out/mathieu.tgz

doc: out/netlist.pdf

clean:
	dune clean
	$(RM) out/*

tests: out/netlist_simulator
	./run-tests

all: out/netlist_simulator

build: out/netlist_simulator out/graph_test out/scheduler_test

.PHONY: all clean install build $(patsubst %,out/%.exe,$TARGETS) tests doc archive
