
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


clean:
	ocaml setup.ml -clean
	$(RM) $(wildcard setup.ml.ba* *.bak)