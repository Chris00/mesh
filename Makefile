TRIANGLE_URL = http://www.netlib.org/voronoi/triangle.zip

PACKAGES = mesh mesh-graphics mesh-easymesh mesh-triangle
PKGVERSION = $(shell git describe --always)
TARBALL = _build/mesh-$(PKGVERSION).tbz

all byte native:
	jbuilder build @install --dev

clean:
	jbuilder clean

doc:
	jbuilder build --dev @doc
	echo '.def { background: #f0f0f0; }' >> _build/default/_doc/odoc.css

tests:
	jbuilder runtest

submit:
	topkg distrib --skip-build --skip-tests
#       Add the Triangle files so the tarball can easily be compiled.
	tar -C _build -xf $(TARBALL)
	mkdir _build/mesh-$(PKGVERSION)/triangle/triangle/
	cp -a triangle/triangle/triangle.* \
	  _build/mesh-$(PKGVERSION)/triangle/triangle/
	tar -C _build -jcf $(TARBALL) mesh-$(PKGVERSION)
	$(RM) -rf _build/mesh-$(PKGVERSION)/
	topkg publish distrib
# 	Create packahes and perform the subtitution that topkkg does not
#	(until opam2, https://discuss.ocaml.org/t/sync-versions-of-several-packages-coming-from-a-single-repo/808/5)
	for p in $(PACKAGES); do \
	  topkg opam pkg -n $$p; \
	  sed -e 's/\(^ *"mesh"\) */\1 {= "$(PKGVERSION)"}/' --in-place \
	  _build/$$p.$(PKGVERSION)/opam; \
	done
# until we have https://github.com/ocaml/opam-publish/issues/38
	[ -d packages ] || (echo "ERROR: Make a symbolic link packages → \
		opam-repo/packages"; exit 1)
	for p in $(PACKAGES); do \
	  mkdir -p packages/$$p; \
	  cp -r _build/$$p.$(PKGVERSION) packages/$$p/; \
	done
	cd packages && git add $(PACKAGES)
#	CONDUIT_TLS=native topkg opam submit $(addprefix -n, $(PACKAGES))



update-triangle:
	@WGET=`which wget`;					\
	UNZIP=`which unzip`;					\
	if [ "x$$WGET" != "x" -a "x$$UNZIP" != "x" ]; then	\
	  mkdir -p triangle/triangle;				\
	  cd triangle/triangle;					\
	  $$WGET $(TRIANGLE_URL);				\
	  $$UNZIP triangle.zip;					\
	  $(RM) triangle.zip;                                   \
	else							\
	  echo "*** Please install wget and unzip.";	        \
	  exit 2;						\
	fi

.PHONY: all byte native doc tests submit update-triangle clean distclean
