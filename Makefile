WEB = mesh.forge.ocamlcore.org:/home/groups/mesh/htdocs/
TRIANGLE_URL = http://www.netlib.org/voronoi/triangle.zip

DIR = $(shell oasis query name)-$(shell oasis query version)
TARBALL = $(DIR).tar.gz

DISTFILES = AUTHORS.txt INSTALL.txt README.txt \
  Makefile myocamlbuild.ml _oasis setup.ml _tags API.odocl \
  src/META $(wildcard src/*.ml src/*.clib src/*.mllib src/*.c src/triangle/*) \
  $(wildcard tests/*.ml)

.PHONY: configure all byte native doc upload-doc install uninstall reinstall
all byte native: setup.data
	ocaml setup.ml -build

setup.data: configure
configure: setup.ml
	@WGET=`which wget`;						     \
	UNZIP=`which unzip`;						     \
	if [ -f "src/triangle/triangle.c" -a -f "src/triangle/triangle.h" ]; \
	then								     \
	  echo "*** Using the Triangle library installed in src/triangle/";  \
	  ocaml setup.ml -configure;					     \
	elif [ -f "/usr/include/triangle.h" ]; then			     \
	  echo "*** Assuming Triangle is installed on the system.";	     \
	  ocaml setup.ml -configure --enable-libtriangle;		     \
	elif [ "x$$WGET" != "x" -a "x$$UNZIP" != "x" ]; then		     \
	  mkdir -p src/triangle;					     \
	  cd src/triangle;						     \
	  $$WGET $(TRIANGLE_URL);					     \
	  $$UNZIP triangle.zip;						     \
	  cd ../..;							     \
	  ocaml setup.ml -configure;					     \
	else								     \
	  echo "*** Please download and install Triangle by hand).";	     \
	  exit 2;							     \
	fi

setup.ml: _oasis
	oasis setup -setup-update dynamic

doc install uninstall reinstall: all
	ocaml setup.ml -$@

upload-doc: doc
	scp -C -p -r _build/API.docdir $(WEB)

.PHONY: dist tar
dist tar: setup.ml
	mkdir -p $(DIR)
	for f in $(DISTFILES); do \
	  cp --parents $$f $(DIR); \
	done
# Generate a setup.ml independent of oasis:
	cd $(DIR); oasis setup
# Download the C lib triangle, so OPAM complilation is smooth:
	mkdir -p $(DIR)/src/triangle;
	cd $(DIR)/src/triangle && wget $(TRIANGLE_URL) \
	&& unzip triangle.zip && rm triangle.zip
	tar -zcvf $(TARBALL) $(DIR)
	$(RM) -r $(DIR)

.PHONY: clean distclean
clean:
	ocaml setup.ml -clean
	$(RM) $(TARBALL)

distclean:
	ocaml setup.ml -distclean
	$(RM) $(wildcard *.ba[0-9] *.bak *~ *.odocl)
