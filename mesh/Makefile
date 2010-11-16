ROOT = ..
DOC_DIR = $(ROOT)/doc
STUBS_LIB = mesh_triangle
DEPEND_EXCLUDE = %FC.ml

INSTALL_FILES = META mesh.cmi mesh.cma mesh.cmxa mesh_triangle.cmi \
	mesh_triangle.cma mesh_triangle.cmx mesh_triangle.cmxa \
	mesh.a mesh_triangle.a \
	$(wildcard dll$(STUBS_LIB).* lib$(STUBS_LIB).*)

INTERFACES = $(wildcard *.mli)

PP = camlp4o pa_macro.cmo
OCAMLC_FLAGS 	= -annot -g
OCAMLOPT_FLAGS 	= -annot
OCAMLDOC_FLAGS 	= -stars -colorize-code

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
	mkdir -p triangle
	wget "http://cm.bell-labs.com/netlib/voronoi/triangle.zip"
	cd triangle && unzip ../triangle.zip

%.o: %.c
	$(OCAMLC) -c -o $@ $<

# Documentation
.PHONY: doc upload-doc
doc:   $(DOC_DIR)/index.html

$(DOC_DIR)/index.html: $(INTERFACES) $(INTERFACES:.mli=.cmi)
	mkdir -p $(DOC_DIR)
	$(OCAMLDOC) -d $(DOC_DIR) -html $(OCAMLDOC_FLAGS) $(INTERFACES)

upload-doc:
	$(MAKE) -C $(ROOT) $@

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
test: test.exe test.com
test.exe: mesh.cma mesh_triangle.cma test.ml
	$(OCAMLC) -o $@ $(OCAMLC_FLAGS) bigarray.cma graphics.cma $^
test.com: mesh.cmxa mesh_triangle.cmxa test.ml
	$(OCAMLOPT) -o $@ $(OCAMLC_FLAGS) bigarray.cmxa graphics.cmxa -I . $^

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
	$(RM) META .depend.ocaml
	$(RM) -r triangle triangle.zip