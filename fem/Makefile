OCAMLMAKEFILE = ../OCamlMakefile
ANNOTATE := yes
OCAMLNCFLAGS = -inline 10

INCDIRS += ../mesh
LIBS += 
PACKS = lacaml

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
