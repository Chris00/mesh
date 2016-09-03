WEB = mesh.forge.ocamlcore.org:/home/groups/mesh/htdocs/
TRIANGLE_URL = http://www.netlib.org/voronoi/triangle.zip

PKG_VERSION = $(shell oasis query version)
DIR = $(shell oasis query name)-$(PKG_VERSION)
TARBALL = $(DIR).tar.gz

DISTFILES = AUTHORS.txt INSTALL.txt README.txt \
  Makefile myocamlbuild.ml _oasis _tags \
  $(wildcard $(addprefix src/, *.ml *.mli *.clib *.mllib *.c triangle/)) \
  $(wildcard tests/*.ml)

.PHONY: configure all byte native doc upload-doc install uninstall reinstall
all byte native: setup.data
	ocaml setup.ml -build

BYTES_DIR = $(shell ocamlfind query bytes)
BYTES_CMA = $(wildcard $(BYTES_DIR)/bytes.cma)
BYTES_INC = $(if $(BYTES_CMA), -I $(BYTES_DIR) $(BYTES_CMA))

GENERATE_ML_FILES = ocaml $(BYTES_INC) src/make_FC_code.ml \
  --pkg-version $(PKG_VERSION)

# For development of the package.
# FIXME: some tests should go to setup.ml
CONFIGURE = ocaml setup.ml -configure --enable-tests --enable-lacaml
setup.data: configure
configure: setup.ml opam/opam
	@WGET=`which wget`;						     \
	UNZIP=`which unzip`;						     \
	if [ -f "src/triangle/triangle.c" -a -f "src/triangle/triangle.h" ]; \
	then								     \
	  echo "*** Using the Triangle library installed in src/triangle/";  \
	  $(CONFIGURE);					     		     \
	elif [ -f "/usr/include/triangle.h" ]; then			     \
	  echo "*** Assuming Triangle is installed on the system.";	     \
	  $(CONFIGURE) --enable-libtriangle;		     		     \
	elif [ "x$$WGET" != "x" -a "x$$UNZIP" != "x" ]; then		     \
	  mkdir -p src/triangle;					     \
	  cd src/triangle;						     \
	  $$WGET $(TRIANGLE_URL);					     \
	  $$UNZIP triangle.zip;						     \
	  cd ../..;							     \
	  $(CONFIGURE);					     		     \
	else								     \
	  echo "*** Please download and install Triangle by hand).";	     \
	  exit 2;							     \
	fi

setup.ml: _oasis
	$(GENERATE_ML_FILES)
	oasis setup -setup-update dynamic

doc install uninstall reinstall: all
	ocaml setup.ml -$@

opam/opam: _oasis
	oasis2opam --local -y

upload-doc: doc
	scp -C -p -r _build/API.docdir $(WEB)

.PHONY: dist tar
dist tar: setup.ml
	mkdir -p $(DIR)
	for f in $(DISTFILES); do \
	  cp --parents $$f $(DIR); \
	done
# Generate a setup.ml independent of oasis:
	cd $(DIR); $(GENERATE_ML_FILES) && oasis setup
# Download the C lib triangle, so OPAM complilation is smooth:
	mkdir -p $(DIR)/src/triangle;
	cd $(DIR)/src/triangle && wget $(TRIANGLE_URL) \
	&& unzip triangle.zip && rm triangle.zip
	tar -zcvf $(TARBALL) $(DIR)
	$(RM) -r $(DIR)

.PHONY: clean distclean
clean:
	ocaml setup.ml -clean
	$(GENERATE_ML_FILES) --clean 
	$(RM) $(TARBALL)

distclean:
	ocaml setup.ml -distclean
	$(RM) $(wildcard src/*.clib src/*.mllib)
	$(RM) $(wildcard *.ba[0-9] *.bak *~ *.odocl)
