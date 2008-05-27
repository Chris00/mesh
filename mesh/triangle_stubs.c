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
#include <triangle.h>

#define INT BIGARRAY_CAML_INT
#define REAL_BIGARRAY_VAL(v) ((REAL *) Data_bigarray_val(v))
#define INT_BIGARRAY_VAL(v) ((long int *) Data_bigarray_val(v))

#if INT_MAX != LONG_MAX
/* if int and long are different, INT_IS_NOT_LONG must be defined */
#define INT_IS_NOT_LONG
#endif

#ifdef INT_IS_NOT_LONG
/* Need to copy betwenn (long *) and (int *) arrays */
#define TRI_INT_OF_BIGARRAY(dest, src, length) \
  n = length; \
  dest = malloc(n * sizeof(int)); \
  p_end = dest + n; \
  for(p = dest, q = src; p < p_end; p++, q++) *p = *q;
/* Free the allocated input */
#define TRI_INT_FREE(v) free(v)
/* Convert back to OCaml and free output */
#define BIGARRAY_OF_TRI_INT_FREE(dest, src, length) \
  n = length; \
  dest = malloc(n * sizeof(long)); \
  p_end = src + n; \
  for(p = src, q = dest; p < p_end; p++, q++) *q = *p;
#else
/* int has the same size as long */
#define TRI_INT_OF_BIGARRAY(dest, src, length) dest = (int *) src
#define TRI_INT_FREE(v)
#define BIGARRAY_OF_TRI_INT_FREE(dest, src, length) dest = (long int *) src
#endif


/* WARNING: Keep in sync with ['l t] Caml data type -- same order */
#define POINT_VAL(v) REAL_BIGARRAY_VAL(Field(v,0))
#define POINT_ARR(v) Bigarray_val(Field(v,0))
#define POINT_ATTRIBUTE_VAL(v) REAL_BIGARRAY_VAL(Field(v,1))
#define POINT_ATTRIBUTE_ARR(v) Bigarray_val(Field(v,1))
#define POINT_MARKER_VAL(v) INT_BIGARRAY_VAL(Field(v,2))
#define POINT_MARKER_ARR(v) Bigarray_val(Field(v,2))

#define TRIANGLE_VAL(v) INT_BIGARRAY_VAL(Field(v,3))
#define TRIANGLE_ARR(v) Bigarray_val(Field(v,3))
#define TRIANGLE_ATTRIBUTE_VAL(v) REAL_BIGARRAY_VAL(Field(v,4))
#define TRIANGLE_ATTRIBUTE_ARR(v) Bigarray_val(Field(v,4))
#define NEIGHBOR_VAL(v) INT_BIGARRAY_VAL(Field(v,5))

#define SEGMENT_VAL(v) INT_BIGARRAY_VAL(Field(v,6))
#define SEGMENT_ARR(v) Bigarray_val(Field(v,6))
#define SEGMENT_MARKER_VAL(v) INT_BIGARRAY_VAL(Field(v,7))
#define SEGMENT_MARKER_ARR(v) Bigarray_val(Field(v,7))

#define HOLE(v) Field(v,8)
#define HOLE_VAL(v) REAL_BIGARRAY_VAL(Field(v,8))
#define HOLE_ARR(v) Bigarray_val(Field(v,8))
#define REGION(v) Field(v,9)
#define REGION_VAL(v) REAL_BIGARRAY_VAL(Field(v,9))
#define REGION_ARR(v) Bigarray_val(Field(v,9))


#define NAME triangulate_c_layout
#define DIM_1ST 0 /* main dim (number of points, triangles,...) */
#define DIM_AUX 1 /* auxiliary dim (number of attrib.,...) */
#define LAYOUT BIGARRAY_C_LAYOUT
#include "triangulate_stub.c"

#define NAME triangulate_fortran_layout
#define DIM_1ST 1
#define DIM_AUX 0
#define LAYOUT BIGARRAY_FORTRAN_LAYOUT
#include "triangulate_stub.c"



#ifdef EXTERNAL_TEST

#define NARGS_TRIUNSUITABLE 7 /* Number of Caml args */
typedef REAL *vertex; /* taken from triangle.c */

int triunsuitable(vertex triorg, vertex tridest, vertex triapex, REAL area)
{
  static value * closure = NULL;
  value args[NARGS_TRIUNSUITABLE];
  if (closure == NULL) {
    closure = caml_named_value("triunsuitable_callback");
  }
  args[0] = triorg[0];
  args[1] = triorg[1];
  args[2] = tridest[0];
  args[3] = tridest[1];
  args[4] = triapex[0];
  args[5] = triapex[1];
  args[6] = area;
  return(Bool_val(callbackN(*closure, NARGS_TRIUNSUITABLE, args)));
}


#endif /* EXTERNAL_TEST */
