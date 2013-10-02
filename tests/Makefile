PKG:=camomile,oUnit

all: test_validate.exe

test_validate.exe: test_validate.ml
	ocamlfind ocamlopt -g -o $@ -package $(PKG) -linkpkg -I ../ ../ocorm.cmx $<

clean:
	rm -f *.cmi *.cmo *.cmx *.cma *.a *.cmxa *.o test_validate

rebuild:
	make clean
	make all