ROOT = ..
STUBS_LIB = mesh_triangle
GENERATED_PATT = easymeshFC.ml

INSTALL_FILES = META mesh.cmi mesh.cma mesh.cmxa \
	$(wildcard dll$(STUBS_LIB).* lib$(STUBS_LIB).*)

PP = camlp4o pa_macro.cmo
OCAMLCFLAGS 	= -annot -g
OCAMLOPTFLAGS 	= -annot
OCAMLDOCFLAGS 	= -stars -colorize-code

VERSION = $(shell grep @version mesh.mli | sed -e "s/ *@version *//")

PKG_NAME =  mesh
SED = sed
LATEX = latex

.PHONY: all byte opt
all: byte opt
byte: mesh.cma mesh_triangle.cma
opt: mesh.cmxa mesh_triangle.cmxa

mesh.cma: mesh.cmo mesh_display.cmo easymesh.cmo
mesh.cmxa: mesh.cmx mesh_display.cmx easymesh.cmx

mesh_triangle.cma mesh_triangle.cmxa: mesh_triangle.cmo mesh_triangle.cmx \
  triangle.o triangle_stubs.o
	$(OCAMLMKLIB) -o $(basename $@) $^

triangle.o: triangle/triangle.c
	$(OCAMLC) -c -o $@ -ccopt -DTRILIBRARY -ccopt -DEXTERNAL_TEST $<

triangle/triangle.c:
	wget "http://cm.bell-labs.com/netlib/voronoi/triangle.zip"
	cd triangle && unzip ../triangle.zip

%.o: %.c
	$(OCAMLC) -c -o $@ $<

# Documentation
.PHONY: doc
doc:   doc/index.html

doc/index.html: $(INTERFACES) $(INTERFACES:.mli=.cmi)
	[ -d doc/ ] || mkdir doc
	$(OCAMLDOC) -d doc -html $(OCAMLDOCFLAGS) $(INTERFACES)

# (Un)install
.PHONY: install uninstall reinstall
install: META
	$(OCAMLFIND) install $(PKG_NAME) $(INSTALL_FILES)
uninstall:
	$(OCAMLFIND) remove $(PKG_NAME)
reinstall:
	$(MAKE) uninstall
	$(MAKE) install

# Test
.PHONY: test
test: test.bc test.opt
test.bc: mesh.cma test.ml
	$(OCAMLC) $(OCAMLCFLAGS) -o $@ -cclib -L. bigarray.cma graphics.cma $^
test.opt: mesh.cmxa test.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ -cclib -L. bigarray.cmxa \
		graphics.cmxa $^

test.exe: mesh.cma test.ml
	$(OCAMLC) -o $@ $(OCAMLCFLAGS) bigarray.cma graphics.cma $^

testmesh.tex: test.bc
	./$<

mesh_display.dvi: mesh_display.tex testmesh.tex
	$(LATEX) $<

########################################################################

include $(ROOT)/Makefile.ocaml

.PHONY: clean
clean::
	-if [ -d ./doc ]; then $(RM) -rf ./doc; fi
	$(RM) $(wildcard *.so)

distclean:
	$(RM) META
#	Files generated for the two layouts:
#	rm easymesh.ml mesh_display.ml
