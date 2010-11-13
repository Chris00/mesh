
all:
	$(MAKE) -C mesh
	$(MAKE) -C fem
	$(MAKE) -C tests

doc:
	$(MAKE) -C mesh $@
