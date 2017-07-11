PACKAGES=core,core_bench,batteries,containers,lwt,lwt.unix
OCAMLOPT=ocamlfind opt -linkpkg -thread

maptest.native: maptest.ml
	$(OCAMLOPT) -package $(PACKAGES) $< -o $@

bufftest.native: bufftest.ml
	$(OCAMLOPT) -package $(PACKAGES) $< -o $@

clean:
	/bin/rm -rf *.native *.cm* *.o
