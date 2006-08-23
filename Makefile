CC	 	= gcc
OCAMLC	 	= ocamlc
OCAMLOPT 	= ocamlopt
OCAMLDEP 	= ocamldep
OCAMLDOC 	= ocamldoc
OCAMLMKLIB 	= ocamlmklib
LIBDIR 		= /usr/local/lib/ocaml/$(shell ocamlc -version)
STUBLIBDIR 	= $(LIBDIR)/stublibs/

OCAMLCFLAGS 	= -dtypes -g
OCAMLOPTFLAGS 	= -dtypes
OCAMLDOCFLAGS 	= -stars -colorize-code

INC_TRIANGLE	= /usr/local/include/
OBJ_TRIANGLE	= /usr/local/lib/triangle/triangle.o

VERSION		= 0.5
SED		= sed
LATEX		= latex

.PHONY: all byte opt
all: byte opt
byte: mesh.cma
opt: mesh.cmxa

mesh.cma: mesh.cmo mesh_display.cmo easymesh.cmo
mesh.cmxa: mesh.cmx mesh_display.cmx easymesh.cmx

easymesh.cmo: easymesh.ml easymesh.cmi

# Generate the files containing the 2 layouts:
easymesh.ml: easymesh.gen.ml
	cp $^ $@
mesh_display.ml: mesh_display.gen.ml
	cp $^ $@

%.gen.ml: %-layout.ml %-common.ml
	$(SED) -e 's/LAYOUT/fortran/g' \
	  -e 's/BA_FIRST/1/g' -e 's/BA_SECOND/2/g' -e 's/BA_THIRD/3/g' \
	  -e 's/BA_LASTCOL(\([^()]\+\))/Array2.dim2(\1)/g' \
	  -e 's/BA_NCOL(\([a-z]\+\))/(Array2.dim2 \1)/g' \
	  -e 's/BA_GET(\([^,()]\+\),\([^,()]\+\),\([^,()]\+\))/\1.{\2,\3}/g' \
	  -e 's/BA_CREATE \([^ ]\+\) \([^ ]\+\) \([^ ]\+\)/Array2.create \1 fortran_layout \2 \3/g' \
	  -e 's/BA_OFIDX(\([^()]\+\))/(\1 - 1)/g' \
	  -e 's/BA_TOIDX(\([^()]\+\))/(\1 + 1)/g' \
	  $< > $@
#	C arrays are transposed.
	$(SED) -e 's/LAYOUT/c/g' \
	  -e 's/BA_FIRST/0/g' -e 's/BA_SECOND/1/g' -e 's/BA_THIRD/2/g' \
	  -e 's/BA_LASTCOL(\([^()]\+\))/Array2.dim1(\1) - 1/g' \
	  -e 's/BA_NCOL(\([a-z]\+\))/(Array2.dim1 \1)/g' \
	  -e 's/BA_GET(\([^,()]\+\),\([^,()]\+\),\([^,()]\+\))/\1.{\3,\2}/g' \
	  -e 's/BA_CREATE \([^ ]\+\) \([^ ]\+\) \([^ ]\+\)/Array2.create \1 c_layout \3 \2/g' \
	  -e 's/BA_OFIDX(\([^()]\+\))/(\1)/g' \
	  -e 's/BA_TOIDX(\([^()]\+\))/(\1)/g' \
	  $< >> $@
	cat $(filter %-common.ml, $^) >> $@

triangle.cma: mesh.cma $(OBJ_TRIANGLE) triangle_stubs.o
	$(OCAMLMKLIB) -o $(@:.cma=) $^ -L$(INC_TRIANGLE)

triangle.cmxa: mesh.cmxa $(OBJ_TRIANGLE) triangle_stubs.o
	$(OCAMLMKLIB) -o $(@:.cmxa=) $^ -L$(INC_TRIANGLE)

triangle_stubs.o: mesh_stubs.c triangle_stub.c


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


.depend: $(wildcard *.mli) \
  $($(filter-out %_gen,$(filter-out %-layout.ml,$(wildcard *.ml)))
	$(OCAMLDEP) $^ > $@

include .depend

########################################################################

.PHONY: clean
clean:
	-rm -f *~ .*~ *.o *.cm[aiox] *.cmxa *.a *.annot
	-rm -f *.log *.aux *.dvi *.ps testmesh.tex
	-rm *.gen.ml
	-find . -type f -perm -u=x -exec rm -f {} \;
	-if [ -d ./doc ]; then rm -rf ./doc; fi

distclean:
#	Files generated for the two layouts:
	rm easymesh.ml mesh_display.ml