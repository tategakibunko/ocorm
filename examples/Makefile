all: obj_dump.exe json_dump.exe

obj_dump.exe: obj_dump.ml
	ocamlfind ocamlopt -g -o $@ -package camomile -linkpkg -I ../ ../ocorm.cmx schema.ml $<

json_dump.exe: json_dump.ml
	ocamlfind ocamlopt -g -o $@ -package camomile,json-wheel -linkpkg -I ../ ../ocorm.cmx schema.ml $<

clean:
	rm -f *.cmi *.cmo *.cmx *.cma *.a *.cmxa *.o obj_dump json_dump

rebuild:
	make clean
	make all