LIBNAME:=ocorm
OPT:=ocamlfind ocamlopt -g
OCAMLC:=ocamlfind ocamlc -g 
PKG:=camomile
LIBSRC:=\
	ocorm.mli ocorm.ml

all: ocorm.cma ocorm.cmxa

ocorm.cma: ocorm.mli ocorm.ml
	$(OCAMLC) -package $(PKG) -o $@ -a $(LIBSRC)

ocorm.cmxa: ocorm.ml
	$(OPT) -package $(PKG) -o $@ -a $(LIBSRC)

install:
	ocamlfind install $(LIBNAME) *.a *.cm[ioxa] *.cmx[as] ocorm.mli META

uninstall:
	ocamlfind remove $(LIBNAME)

reinstall:
	make uninstall
	make install

clean:
	rm -f *.cmi *.cmo *.cmx *.cma *.a *.cmxa *.o

rebuild:
	make clean
	make all