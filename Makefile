
.PHONY: all byte native doc upload-doc install uninstall reinstall
all byte native:
	$(MAKE) -C src
	$(MAKE) -C tests

doc install uninstall reinstall:
	$(MAKE) -C src $@

upload-doc: doc


clean:
	$(MAKE) -C src $@
	$(MAKE) -C tests $@