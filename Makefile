WEB = mesh.forge.ocamlcore.org:/home/groups/mesh/htdocs/

.PHONY: configure all byte native doc upload-doc install uninstall reinstall
configure: setup.ml
	ocaml setup.ml -configure

all byte native: configure
	ocaml setup.ml -build

setup.ml:
	oasis setup

doc install uninstall reinstall:
	ocaml setup.ml -$@

upload-doc: doc
	scp -C -p -r _build/API.docdir $(WEB)

clean:
	ocaml setup.ml -clean
	$(RM) $(wildcard *.ba[0-9] *.bak *~) setup.log