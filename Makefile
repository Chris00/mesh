TRIANGLE_URL = http://www.netlib.org/voronoi/triangle.zip

all byte native:
	jbuilder build @install --dev

clean:
	jbuilder clean

doc:
	jbuilder build --dev @doc




GENERATE_ML_FILES = ocaml $(BYTES_INC) src/make_FC_code.ml \
  --pkg-version $(PKG_VERSION)

# For development of the package.
# FIXME: some tests should go to setup.ml
CONFIGURE = ocaml setup.ml -configure --enable-tests --enable-lacaml
setup.data: configure
configure: setup.ml opam/opam
	@WGET=`which wget`;						     \
	UNZIP=`which unzip`;						     \
	if [ -f "src/triangle/triangle.c" -a -f "src/triangle/triangle.h" ]; \
	then								     \
	  echo "*** Using the Triangle library installed in src/triangle/";  \
	  $(CONFIGURE);					     		     \
	elif [ -f "/usr/include/triangle.h" ]; then			     \
	  echo "*** Assuming Triangle is installed on the system.";	     \
	  $(CONFIGURE) --enable-libtriangle;		     		     \
	elif [ "x$$WGET" != "x" -a "x$$UNZIP" != "x" ]; then		     \
	  mkdir -p src/triangle;					     \
	  cd src/triangle;						     \
	  $$WGET $(TRIANGLE_URL);					     \
	  $$UNZIP triangle.zip;						     \
	  cd ../..;							     \
	  $(CONFIGURE);					     		     \
	else								     \
	  echo "*** Please download and install Triangle by hand).";	     \
	  exit 2;							     \
	fi

upload-doc: doc
	scp -C -p -r _build/API.docdir $(WEB)

.PHONY: all byte native doc clean distclean
