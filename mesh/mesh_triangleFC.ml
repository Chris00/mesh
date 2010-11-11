
(* check that all C "triexit" have been avoided. *)

let triangulate ?point_attribute ?(refine=true) ?triangle_attribute
    ?min_angle ?max_area
    ?(convex_hull=false) ?max_steiner ?(voronoi=false) ?(edge=false)
    ?(subparam=false) ?triangle_area mesh =
  (* Check points *)
  if NROWS(mesh.point) <> 2 then
    invalid_arg("triangle: " ^ ROWS ^ " mesh.point <> 2");
  if NCOLS(mesh.point_attribute) > 0
    && NCOLS(mesh.point_attribute) < NCOLS(mesh.point) then
    invalid_arg("triangle: " ^ COLS ^ " mesh.point_attribute < "
                ^ COLS ^ " mesh.point");
  if Array1.dim mesh.point_marker > 0
    && Array1.dim mesh.point_marker < NCOLS(mesh.point) then
    invalid_arg("triangle: dim mesh.point_marker < " ^ COLS ^ " mesh.point");
  (* Check for PSLG *)
  let switches =
    if pslg then begin
      if dim1 mesh.segment > 0 then begin
        if dim2 mesh.segment <> 2 then invalid_arg(sdim2 ^ " segment <> 2");
        if Array1.dim mesh.segment_marker > 0
          && Array1.dim mesh.segment_marker < dim1 mesh.segment then
          invalid_arg("dim segment_marker < " ^ sdim1 ^ " segment");
      end;
      if not refine then begin
        if dim1 mesh.hole > 0 && dim2 mesh.hole <> 2 then
          invalid_arg(sdim2 ^ " hole <> 2");
        if dim1 mesh.region > 0 && dim2 mesh.region <> 4 then
          invalid_arg(sdim2 ^ " region <> 4");
      end;
      switches ^ "p"
    end else switches in
  (* Check for refinement -- triangles *)
  let switches =
    if refine then begin
      if dim1 mesh.triangle > 0 then begin
        if dim2 mesh.triangle <> 3 && dim2 mesh.triangle <> 6 then
          invalid_arg(sdim2 ^ " triangle must be 3 or 6");
        if dim2 mesh.triangle_attribute > 0
          && dim1 mesh.triangle_attribute < dim1 mesh.triangle then
          invalid_arg(sdim1 ^ " triangle_attribute < "
                      ^ sdim1 ^ " triangle");
      end;
      switches ^ "r"
    end else switches in
  (* Check triangle_area *)
  let switches, triangle_area = match triangle_area with
    | None -> switches, Array1.create prec layout 0
    | Some a ->
      if Array1.dim a < dim1 mesh.triangle then
        invalid_arg("dim triangle_area < " ^ sdim1 ^ " triangle");
      (switches ^ "a", a) in
  (* Other switches *)
  let switches = match min_angle with
    | None -> switches
    | Some a -> switches ^ "q" ^ (string_of_float a) in
  let switches = match max_area with
    | None -> switches
    | Some a -> switches ^ "a" ^ (string_of_float a) in
  let switches = if convex_hull then switches ^ "c" else switches in
  let switches = match max_steiner with
    | None -> switches
    | Some a -> switches ^ "S" ^ (string_of_int a) in
  let switches = if voronoi then switches ^ "v" else switches in
  let switches = if edge then switches ^ "e" else switches in
  let switches = if subparam then switches ^ "o2" else switches in
  (* Call triangle and build the resulting objects *)
  let point, point_attribute, point_marker, triangle, triangle_attribute,
    neighbor, segment, segment_marker, edge, edge_marker,
    vor_point, vor_point_attribute, vor_edge, vor_normal =
    triangle switches mesh triangle_area in
  let mesh_out : layout mesh =
    (object
      method point               = point
      method point_attribute     = point_attribute
      method point_marker        = point_marker
      method triangle            = triangle
      method triangle_attribute  = triangle_attribute
      method neighbor            = neighbor
      method segment             = segment
      method segment_marker      = segment_marker
      method edge                = edge
      method edge_marker         = edge_marker
      method hole = mesh#hole
      method region = mesh#region
     end)
  and vor : layout voronoi =
    (object
      method point               = vor_point
      method point_attribute     = vor_point_attribute
      method edge                = vor_edge
      method normal              = vor_normal
     end) in
  (mesh_out, vor)
