
subdotter.exe: FORCE
	@dune build $(DUNEARGS) subdotter.exe

install: FORCE
	@dune install

uninstall: FORCE
	@dune uninstall

clean:
	@dune $(DUNEARGS) clean

cleanall: clean

FORCE: ;
