
.PHONY: all byte native doc upload-doc
all byte native:
	$(MAKE) -C src
	$(MAKE) -C tests

doc:
	$(MAKE) -C mesh $@

upload-doc: doc

