
.PHONY: all byte native doc upload-doc
all byte native:
	$(MAKE) -C mesh
	$(MAKE) -C fem
	$(MAKE) -C tests

doc:
	$(MAKE) -C mesh $@

upload-doc: doc
	scp -r doc "$$SFUSER,ocaml-fem@web.sourceforge.net:htdocs"
