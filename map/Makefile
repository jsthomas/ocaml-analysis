PACKAGES=core,core_bench,batteries,containers,lwt,lwt.unix
OPAM_PACKAGES=core core_bench batteries containers lwt
CC=ocamlfind opt -linkpkg -thread

maptest.native: maptest.ml
	$(CC) -package $(PACKAGES) $< -o $@

bufftest.native: bufftest.ml
	$(CC) -package $(PACKAGES) $< -o $@

depends:
	opam install $(OPAM_PACKAGES)

clean:
	/bin/rm -rf *.native *.cm* *.o
