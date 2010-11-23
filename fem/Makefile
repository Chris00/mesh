OCAMLMAKEFILE = ../OCamlMakefile
ANNOTATE := yes
OCAMLNCFLAGS = -inline 10

LIBS += 
PACKS = lacaml,mesh

SOURCES = laplacian.ml
RESULT = laplacian

.PHONY: all byte opt install uninstall update
all: byte opt #@ALL@
byte: byte-code-library
opt: native-code-library

install: libinstall
uninstall: libuninstall
update: uninstall install

include $(OCAMLMAKEFILE)
