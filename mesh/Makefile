ROOT = ..
DOC_DIR = $(ROOT)/doc
STUBS_LIB = mesh_triangle
DEPEND_EXCLUDE = %FC.ml

INTERFACES = $(wildcard *.mli)

INSTALL_FILES = META $(INTERFACES) $(INTERFACES:.mli=.cmi) mesh.cma mesh.cmxa \
	mesh_display.cma mesh_display.cmx mesh_display.cmxa \
	mesh_triangle.cma mesh_triangle.cmx mesh_triangle.cmxa \
	mesh.a mesh_display.a mesh_triangle.a \
	$(wildcard dll$(STUBS_LIB).* lib$(STUBS_LIB).*)

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
byte: mesh.cma mesh_triangle.cma mesh_display.cma
opt: mesh.cmxa mesh_triangle.cmxa mesh_display.cmxa

mesh.cma: mesh.cmo easymesh.cmo
mesh.cmxa: mesh.cmx easymesh.cmx
mesh_display.cma: mesh_display.cmo
mesh_display.cmxa: mesh_display.cmx

mesh.cmo mesh.cmx: meshFC.ml
easymesh.cmo easymesh.cmx: easymeshFC.ml
mesh_triangle.cmo mesh_triangle.cmx: mesh_triangleFC.ml

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
test.exe: mesh.cma mesh_triangle.cma mesh_display.cma test.ml
	$(OCAMLC) -o $@ $(OCAMLC_FLAGS) bigarray.cma graphics.cma $^
test.com: mesh.cmxa mesh_triangle.cmxa mesh_display.cmxa test.ml
	$(OCAMLOPT) -o $@ $(OCAMLC_FLAGS) bigarray.cmxa graphics.cmxa -I . $^

TEST_DATA = testmesh.tex testmesh1.tex
$(TEST_DATA): test.com
	./$<

test.pdf: test.tex $(TEST_DATA)
	pdflatex $<

mesh_display.pdf: mesh_display.tex testmesh.tex
	$(PDFLATEX) $<

########################################################################

include $(ROOT)/Makefile.ocaml

.PHONY: clean
clean::
	-if [ -d ./doc ]; then $(RM) -rf ./doc; fi
	$(RM) $(wildcard *.so) triangle.sci levels.tex .depend.ocaml
	$(RM) $(wildcard testmesh*.tex)

distclean:
	$(RM) META
	$(RM) -r triangle triangle.zip