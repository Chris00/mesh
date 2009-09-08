CAMLexport value NAME(value switches,
                      value mesh_in,
                      value triangle_area)
{
  CAMLparam3(switches, mesh_in, triangle_area);
  CAMLlocal3(mesh_out, vor_out, tuple);
  struct triangulateio in, out, vor;
  long int dims[2];
#ifdef INT_IS_NOT_LONG
  /* Needed by TRI_INT_OF_BIGARRAY and BIGARRAY_OF_TRI_INT */
  int *p, *p_end, n;
  long int *q;
#endif

  /* IN structure */
  in.pointlist               = POINT_VAL(mesh_in);
  in.pointattributelist      = POINT_ATTRIBUTE_VAL(mesh_in);
  if (POINT_MARKER_ARR(mesh_in)->dim[0] == 0)
    in.pointmarkerlist       = NULL;
  else
    TRI_INT_OF_BIGARRAY(in.pointmarkerlist, POINT_MARKER_VAL(mesh_in),
                        POINT_MARKER_ARR(mesh_in)->dim[0]);
  in.numberofpoints          = POINT_ARR(mesh_in)->dim[DIM_1ST];
  in.numberofpointattributes = POINT_ATTRIBUTE_ARR(mesh_in)->dim[DIM_AUX];

  in.numberoftriangles       = TRIANGLE_ARR(mesh_in)->dim[DIM_1ST];
  in.numberofcorners         = TRIANGLE_ARR(mesh_in)->dim[DIM_AUX];
  in.numberoftriangleattributes =
    TRIANGLE_ATTRIBUTE_ARR(mesh_in)->dim[DIM_AUX];
  TRI_INT_OF_BIGARRAY(in.trianglelist, TRIANGLE_VAL(mesh_in),
                      in.numberoftriangles * in.numberofcorners);
  in.triangleattributelist   = TRIANGLE_ATTRIBUTE_VAL(mesh_in);
  in.trianglearealist        = REAL_BIGARRAY_VAL(triangle_area);

  in.numberofsegments        = SEGMENT_ARR(mesh_in)->dim[DIM_1ST];
  TRI_INT_OF_BIGARRAY(in.segmentlist, SEGMENT_VAL(mesh_in),
                      in.numberofsegments * 2);
  if (SEGMENT_MARKER_ARR(mesh_in)->dim[0] == 0)
    in.segmentmarkerlist     = NULL;
  else
    TRI_INT_OF_BIGARRAY(in.segmentmarkerlist, SEGMENT_MARKER_VAL(mesh_in),
                        in.numberofsegments);

  in.holelist                = HOLE_VAL(mesh_in);
  in.numberofholes           = HOLE_ARR(mesh_in)->dim[DIM_1ST];
  in.regionlist              = REGION_VAL(mesh_in);
  in.numberofregions         = REGION_ARR(mesh_in)->dim[DIM_1ST];
  /* in.neighborlist in.edgelist in.edgemarkerlist in.normlist
   * in.numberofedges ignored */

  /* OUT structure */
  out.pointlist = NULL;
  out.pointattributelist = NULL;
  out.pointmarkerlist = NULL;
  out.numberofpoints = 0;
  out.numberofpointattributes = 0;
  out.trianglelist = NULL;
  out.triangleattributelist = NULL;
  out.numberoftriangles = 0;
  out.numberofcorners = 0;
  out.numberoftriangleattributes = 0;
  out.neighborlist = NULL;
  out.segmentlist = NULL;
  out.segmentmarkerlist = NULL;
  out.numberofsegments = 0;
  out.edgelist = NULL;
  out.edgemarkerlist = NULL;
  out.numberofedges = 0;

  /* Voronoi structure */
  vor.pointlist = NULL;
  vor.pointattributelist = NULL;
  vor.numberofpoints = 0;
  vor.edgelist = NULL;
  vor.normlist = NULL;
  vor.numberofedges = 0;

  /* Call [triangle] */
  triangulate(String_val(switches), &in, &out, &vor);

  /* Create a Caml structure from [out] */
  mesh_out = alloc(12, 0);
  dims[DIM_1ST] = out.numberofpoints;
  dims[DIM_AUX] = 2;
  Store_field(mesh_out, 0, /* point */
              alloc_bigarray(PREC | LAYOUT | BIGARRAY_MANAGED,
                             2, BIGARRAY_OF_TRI_INT(out.pointlist), dims));
  dims[DIM_AUX] = out.numberofpointattributes;
  Store_field(mesh_out, 1, /* point_attribute */
              alloc_bigarray(PREC | LAYOUT, 2, out.pointattributelist, dims));
  dims[0] = out.numberofpoints;
  Store_field(mesh_out, 2, /* point_marker */
              alloc_bigarray(INT | LAYOUT, 1, out.pointmarkerlist, dims));
  dims[DIM_1ST] = out.numberoftriangles;
  dims[DIM_AUX] = out.numberofcorners;
  Store_field(mesh_out, 3, /* triangle */
              alloc_bigarray(INT | LAYOUT, 2, out.trianglelist, dims));
  dims[DIM_AUX] = out.numberoftriangleattributes;
  Store_field(mesh_out, 4, /* triangle_attribute */
              alloc_bigarray(PREC | LAYOUT, 2,
                             out.triangleattributelist, dims));
  if (out.neighborlist == NULL)
    dims[DIM_1ST] = 0;
  dims[DIM_AUX] = 3;
  Store_field(mesh_out, 5, /* neighbor */
              alloc_bigarray(INT | LAYOUT, 2, out.trianglelist, dims));
  dims[DIM_1ST] = out.numberofsegments;
  dims[DIM_AUX] = 2;
  Store_field(mesh_out, 6, /* segment */
              alloc_bigarray(INT | LAYOUT, 2, out.segmentlist, dims));
  dims[0] = out.numberofsegments;
  Store_field(mesh_out, 7, /* segment_marker */
              alloc_bigarray(INT | LAYOUT, 1, out.segmentmarkerlist, dims));
  Store_field(mesh_out, 8, /* hole */ HOLE(mesh_in));
  Store_field(mesh_out, 9, /* region */ REGION(mesh_in));
  dims[DIM_1ST] = out.numberofedges;
  dims[DIM_AUX] = 2;
  Store_field(mesh_out, 10, /* edge */
              alloc_bigarray(INT | LAYOUT, 2, out.edgelist, dims));
  dims[0] = out.numberofedges;
  Store_field(mesh_out, 1, /* edge_marker */
              alloc_bigarray(INT | LAYOUT, 1, out.edgemarkerlist, dims));

  /* Create a Caml structure from [vor] */
  vor_out = alloc(4, 0);
  dims[DIM_1ST] = vor.numberofpoints;
  dims[DIM_AUX] = 2;
  Store_field(vor_out, 0, /* point */
              alloc_bigarray(PREC | LAYOUT, 2, vor.pointlist, dims));
  dims[DIM_AUX] = vor.numberofpointattributes;
  Store_field(vor_out, 1, /* point_attribute */
              alloc_bigarray(PREC | LAYOUT, 2, vor.pointattributelist, dims));
  dims[DIM_1ST] = vor.numberofedges;
  dims[DIM_AUX] = 2;
  Store_field(vor_out, 2, /* edge */
              alloc_bigarray(INT | LAYOUT, 2, vor.edgelist, dims));
  Store_field(vor_out, 3, /* normal */
              alloc_bigarray(PREC | LAYOUT, 2, vor.normlist, dims));

  /* Create a Caml tuple */
  tuple = alloc(2, 0);
  Store_field(tuple, 0, mesh_out);
  Store_field(tuple, 1, vor_out);

  CAMLreturn(tuple);
}

#undef NAME
#undef DIM_1ST
#undef DIM_AUX
#undef LAYOUT
