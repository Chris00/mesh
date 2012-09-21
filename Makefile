WEB = mesh.forge.ocamlcore.org:/home/groups/mesh/htdocs/

DIR = $(shell oasis query name)-$(shell oasis query version)
TARBALL = $(DIR).tar.gz

DISTFILES = AUTHORS.txt INSTALL.txt README.txt \
  Makefile myocamlbuild.ml _oasis setup.ml _tags API.odocl \
  src/META $(wildcard src/*.ml src/*.clib src/*.mllib src/*.c src/triangle/*) \
  $(wildcard tests/*.ml)

.PHONY: configure all byte native doc upload-doc install uninstall reinstall
all byte native: setup.data
	ocaml setup.ml -build

configure: setup.data
setup.data: setup.ml
	ocaml setup.ml -configure
	$(MAKE) -C src triangle/triangle.c triangle/triangle.h

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
	tar -zcvf $(TARBALL) $(DIR)
	$(RM) -r $(DIR)

.PHONY: clean distclean
clean:
	ocaml setup.ml -clean
	$(RM) $(TARBALL)

distclean:
	ocaml setup.ml -distclean
	$(RM) $(wildcard *.ba[0-9] *.bak *~ *.odocl)
