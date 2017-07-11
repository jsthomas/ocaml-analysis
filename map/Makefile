PACKAGES=core,core_bench,batteries,containers,lwt,lwt.unix
OCAMLOPT=ocamlfind opt -thread

maptest.native: maptest.ml
	$(OCAMLOPT) -package $(PACKAGES) -c batt_hybr.ml
	$(OCAMLOPT) -linkpkg -package $(PACKAGES) batt_hybr.cmx $< -o $@

bufftest.native: bufftest.ml
	$(OCAMLOPT) -linkpkg -package $(PACKAGES) $< -o $@

clean:
	/bin/rm -rf *.native *.cm* *.o
