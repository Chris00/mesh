ROOT = ..
STUBS_LIB = mesh_triangle
DEPEND_EXCLUDE = easymeshFC.ml

INSTALL_FILES = META mesh.cmi mesh.cma mesh.cmxa mesh_triangle.cmi \
	mesh_triangle.cma mesh_triangle.cmx mesh_triangle.cmxa \
	$(wildcard dll$(STUBS_LIB).* lib$(STUBS_LIB).*)

PP = camlp4o pa_macro.cmo
OCAMLCFLAGS 	= -annot -g
OCAMLOPTFLAGS 	= -annot
OCAMLDOCFLAGS 	= -stars -colorize-code

VERSION = $(shell grep @version mesh.mli | sed -e "s/ *@version *//")

PKG_NAME =  mesh
SED = sed
PDFLATEX = pdflatex

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
install: META all
	$(OCAMLFIND) install $(PKG_NAME) $(INSTALL_FILES)
uninstall:
	$(OCAMLFIND) remove $(PKG_NAME)
reinstall:
	$(MAKE) uninstall
	$(MAKE) install

META: META.in mesh.mli
	sed -e "s/@PACKAGE_VERSION@/$(VERSION)/" $< > $@

# Test
.PHONY: test
test: test.bc test.opt
test.bc: test.ml byte
	$(OCAMLC) $(OCAMLCFLAGS) -o $@ -cclib -L. bigarray.cma graphics.cma $^
test.opt: test.ml native
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ -cclib -L. bigarray.cmxa \
		graphics.cmxa $^

test.exe: mesh.cma mesh_triangle.cma test.ml
	$(OCAMLC) -o $@ $(OCAMLCFLAGS) bigarray.cma graphics.cma $^
test.com: mesh.cmxa mesh_triangle.cmxa test.ml
	$(OCAMLOPT) -o $@ $(OCAMLCFLAGS) bigarray.cmxa graphics.cmxa -I . $^

testmesh.tex: test.bc
	./$<

mesh_display.pdf: mesh_display.tex testmesh.tex
	$(PDFLATEX) $<

########################################################################

include $(ROOT)/Makefile.ocaml

.PHONY: clean
clean::
	-if [ -d ./doc ]; then $(RM) -rf ./doc; fi
	$(RM) $(wildcard *.so) triangle.sci levels.tex

distclean:
	$(RM) META
