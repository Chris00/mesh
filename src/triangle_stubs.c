/* Stub functions to access the triangle meshing library from Ocaml */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/bigarray.h>

#include <stdlib.h>
#include <limits.h>

#ifdef SINGLE
#define REAL float
#define PREC BIGARRAY_FLOAT32
#else
#define REAL double
#define PREC BIGARRAY_FLOAT64
#endif /* SINGLE */

#ifdef DEBIAN
#include "triangle.h"
typedef REAL *vertex; /* taken from triangle.c */
#else
/* Include triangle code; cleaner to switch to an external library if
   it exists. */
#include "triangle/triangle.c"
#endif

#define REAL_BIGARRAY_VAL(v) ((REAL *) Data_bigarray_val(v))
#define INT_BIGARRAY_VAL(v) ((long int *) Data_bigarray_val(v))

/* WARNING: Keep in sync with ['l Mesh.t] Caml data type */
static value meth_point;
static value meth_point_attribute;
static value meth_point_marker;
static value meth_triangle;
static value meth_triangle_attribute;
static value mesh_segment;
static value meth_segment_marker;
static value meth_hole;
static value meth_region;

value ocaml_triangle_init(value vunit)
{
  /* noalloc */
  meth_point = hash_variant("point");
  meth_point_attribute = hash_variant("point_attribute");
  meth_point_marker = hash_variant("point_marker");
  meth_triangle = hash_variant("triangle");
  meth_triangle_attribute = hash_variant("triangle_attribute");
  mesh_segment = hash_variant("segment");
  meth_segment_marker = hash_variant("segment_marker");
  meth_hole = hash_variant("hole");
  meth_region = hash_variant("region");
  return Val_unit;
}


#define METHOD(v, m) callback(caml_get_public_method(v, m), v)

#define BA_METHOD(v, m) (Bigarray_val(METHOD(v, m)))
#define VEC_OF_BA(ba) ((REAL *) ba->data)
#define MAT_OF_BA(ba) ((REAL *) ba->data)

#define COPY_INT_BA(a, ba, dim)                \
  if (dim == 0)                                \
    a = NULL;                                  \
  else {                                       \
    int *dest;                                  \
    intnat *src = ba->data;                     \
    intnat *end = src + dim;                    \
    a = (int *) malloc(dim * sizeof(int));      \
    dest = a;                                   \
    for(; src < end; src++) {                   \
      *dest = *src;                             \
      dest++;                                   \
    }                                           \
  }

/* Copy int a[] into the bigarray vba and free a.
   WARNING:  This can modify dims[DIM_1ST]. */
#define COPY_BA_INT(vba, numdims, a, dims) do{                          \
  int length = dims[0];                                                 \
  if (numdims == 2) length *= dims[1];                                  \
  if (a == NULL || length == 0) {                                       \
    if (numdims == 1)                                                   \
      vba = alloc_bigarray_dims(BIGARRAY_CAML_INT | LAYOUT, 1, NULL, 0); \
    else {                                                              \
      dims[DIM_1ST] = 0;                                                 \
      vba = alloc_bigarray(BIGARRAY_CAML_INT | LAYOUT, numdims, NULL, dims); \
    }                                                                   \
  }                                                                     \
  else {                                                                \
    int *src, *end;                                                     \
    intnat *dest;                                                       \
    vba = alloc_bigarray(BIGARRAY_CAML_INT | LAYOUT, numdims, NULL, dims); \
    end = a + length;                                                   \
    dest = (intnat *) Data_bigarray_val(vba);                           \
    for(src = a; src < end; src++) {                                    \
      *dest = *src;                                                     \
      dest++;                                                           \
    }                                                                   \
    free(a);                                                            \
  }                                                                     \
  } while(0)



#define NAME triangulate_fortran_layout
#define DIM_1ST 0
#define DIM_2ND 1 /* main dim (number of points, triangles,...) */
#define LAYOUT BIGARRAY_FORTRAN_LAYOUT
#include "triangulate_stub.c"

#define NAME triangulate_c_layout
#define DIM_1ST 1 /* main dim (number of points, triangles,...) */
#define DIM_2ND 0 /* auxiliary dim (number of attrib.,...) */
#define LAYOUT BIGARRAY_C_LAYOUT
#include "triangulate_stub.c"


#define NARGS_TRIUNSUITABLE 7 /* Number of Caml args */

CAMLexport
int triunsuitable(vertex triorg, vertex tridest, vertex triapex, REAL area)
{
  CAMLparam0();
  CAMLlocal1(vd);
  static value * closure = NULL;
  value args[NARGS_TRIUNSUITABLE];
  if (closure == NULL) {
    closure = caml_named_value("triunsuitable_callback");
  }

#define COPY_DOUBLE(dest, d) \
  vd = caml_copy_double(d);  \
  dest = vd

  COPY_DOUBLE(args[0], triorg[0]);
  COPY_DOUBLE(args[1], triorg[1]);
  COPY_DOUBLE(args[2], tridest[0]);
  COPY_DOUBLE(args[3], tridest[1]);
  COPY_DOUBLE(args[4], triapex[0]);
  COPY_DOUBLE(args[5], triapex[1]);
  COPY_DOUBLE(args[6], area);
  CAMLreturn(Bool_val(callbackN(*closure, NARGS_TRIUNSUITABLE, args)));

#undef COPY_DOUBLE
}
