CC	 	= gcc
OCAMLC	 	= ocamlc
OCAMLOPT 	= ocamlopt
OCAMLDEP 	= ocamldep
OCAMLDOC 	= ocamldoc
OCAMLMKLIB 	= ocamlmklib
LIBDIR 		= /usr/local/lib/ocaml/$(shell ocamlc -version)
STUBLIBDIR 	= $(LIBDIR)/stublibs/

PP = -pp "camlp4o pa_macro.cmo"
OCAMLCFLAGS 	= -dtypes -g $(PP)
OCAMLOPTFLAGS 	= -dtypes $(PP)
OCAMLDOCFLAGS 	= -stars -colorize-code

VERSION		= 0.6
SED		= sed
LATEX		= latex

.PHONY: all byte opt
all: byte opt
byte: mesh.cma
opt: mesh.cmxa

mesh.cma: mesh.cmo mesh_display.cmo easymesh.cmo
mesh.cmxa: mesh.cmx mesh_display.cmx easymesh.cmx

mesh.cmo: mesh.ml mesh.cmi
mesh.cmx: mesh.ml mesh.cmi
easymesh.cmo: easymesh.ml easymesh.cmi mesh.cmo
easymesh.cmx: easymesh.ml easymesh.cmi mesh.cmx
mesh_display.cmo: mesh_display.ml mesh_display.cmi mesh.cmo
mesh_display.cmx: mesh_display.ml mesh_display.cmi mesh.cmx

triangle.cma: mesh.cma triangle/triangle.o triangle_stubs.o
	$(OCAMLMKLIB) -o $(@:.cma=) $^

triangle.cmxa: mesh.cmxa triangle/triangle.o triangle_stubs.o
	$(OCAMLMKLIB) -o $(@:.cmxa=) $^

triangle/triangle.o: triangle/triangle.c
	$(CC) -c -o $@ -DTRILIBRARY $<

triangle/triangle.c:
	wget http://cm.bell-labs.com/netlib/voronoi/triangle.zip
	cd triangle && unzip ../triangle.zip

# Tests

test.exe: mesh.cma test.ml
	$(OCAMLC) -o $@ $(OCAMLCFLAGS) bigarray.cma graphics.cma $^

# Documentation

doc:   doc/index.html

INTERFACES = $(wildcard *.mli)
doc/index.html: $(INTERFACES) $(INTERFACES:.mli=.cmi)
	[ -d doc/ ] || mkdir doc
	$(OCAMLDOC) -d doc -html $(OCAMLDOCFLAGS) $(INTERFACES)
#	Setup the $VERSION strings
	cd doc; \
	for f in *; do \
		cp $$f $$f.bak; \
		$(SED) -e 's/$$VERSION/$(VERSION)/' < $$f.bak > $$f; \
		rm -f $$f.bak; \
	done

# (Un)install
.PHONY: install install-byte install-opt uninstall
install: install-byte install-opt
install-byte: byte mesh.cmi
	[ -d $(LIBDIR) ] || mkdir -p $(LIBDIR)
	cp mesh.cma mesh.cmi $(LIBDIR)
	[ -d $(STUBLIBDIR) ] || mkdir -p $(STUBLIBDIR)
	cp dllmesh.so $(STUBLIBDIR)
install-opt: opt mesh.cmi
	[ -d $(LIBDIR) ] || mkdir -p $(LIBDIR)
	cp mesh.cmxa mesh.cmi $(LIBDIR)
	[ -d $(STUBLIBDIR) ] || mkdir -p $(STUBLIBDIR)
	cp dllmesh.so $(STUBLIBDIR)
uninstall:

# Test
.PHONY: test
test: test.bc test.opt
test.bc: mesh.cma test.ml
	$(OCAMLC) $(OCAMLCFLAGS) -o $@ -cclib -L. bigarray.cma graphics.cma $^
test.opt: mesh.cmxa test.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ -cclib -L. bigarray.cmxa \
		graphics.cmxa $^

testmesh.tex: test.bc
	./$<

mesh_display.dvi: mesh_display.tex testmesh.tex
	$(LATEX) $<

# Generic rules
%.cmi: %.mli
	$(OCAMLC) $(OCAMLCFLAGS) -c $<
%.cmo: %.ml
	$(OCAMLC) $(OCAMLCFLAGS) -c $<
%.cma: %.cmo
	$(OCAMLC) $(OCAMLCFLAGS) -a -o $@ $^
%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<
%.cmxa: %.cmx
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $@ $^


.depend: $(wildcard *.mli) $(filter-out %FC.ml,$(wildcard *.ml))
	$(OCAMLDEP) $(PP) $^ > $@

include .depend

########################################################################

.PHONY: clean
clean:
	-rm -f *~ .*~ *.o *.cm[aiox] *.cmxa *.a *.annot
	-rm -f *.log *.aux *.dvi *.ps testmesh.tex
	-rm *.gen.ml
	-find . -type f -perm -u=x -exec rm -f {} \;
	-if [ -d ./doc ]; then rm -rf ./doc; fi
	-$(MAKE) -C triangle distclean

distclean:
#	Files generated for the two layouts:
#	rm easymesh.ml mesh_display.ml