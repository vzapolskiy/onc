# Simple compilation of onc program
PROGRAM  = onc
PACKAGES = unix,getopt
METHOD   = native

all: $(PROGRAM)

$(PROGRAM): $(PROGRAM).$(METHOD)
	mv $^ $(PROGRAM)

$(PROGRAM).byte: $(PROGRAM).cmo
	ocamlfind ocamlc -package $(PACKAGES) -linkpkg $^ -o $@

$(PROGRAM).cmo: $(PROGRAM).ml
	ocamlfind ocamlc -c -package $(PACKAGES) $^ -o $@

$(PROGRAM).native: $(PROGRAM).cmx
	ocamlfind ocamlopt -package $(PACKAGES) -linkpkg $^ -o $@

$(PROGRAM).cmx $(PROGRAM).o: $(PROGRAM).ml
	ocamlfind ocamlopt -c -package $(PACKAGES) $^ -o $@

clean:
	rm -f $(PROGRAM) $(PROGRAM).cmo $(PROGRAM).cmx $(PROGRAM).cmi $(PROGRAM).o

.PHONY: clean $(PROGRAM).byte $(PROGRAM).native
