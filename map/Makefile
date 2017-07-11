PACKAGES=core,core_bench,batteries,containers,lwt,lwt.unix
CC=ocamlfind opt -linkpkg -thread

maptest.native: maptest.ml
	$(CC) -package $(PACKAGES) $< -o $@

bufftest.native: bufftest.ml
	$(CC) -package $(PACKAGES) $< -o $@

clean:
	/bin/rm -rf *.native
