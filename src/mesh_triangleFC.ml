
(* check that all C "triexit" have been avoided. *)

let triangulate ?(delaunay=true) ?min_angle ?max_area
    ?max_steiner ?(voronoi=false) ?(edge=true) ?(neighbor=false)
    ?(subparam=false) ?triangle_area ?triunsuitable ?(debug=true)
    ~pslg ~refine (mesh: layout t) =
  (* Check points *)
  let point = mesh#point in
  if NROWS(point) <> 2 then invalid_arg(ROWS ^ " mesh#point <> 2");
  if NCOLS(mesh#point_attribute) > 0
    && NCOLS(mesh#point_attribute) < NCOLS(point) then
    invalid_arg(COLS ^ " mesh#point_attribute < " ^ COLS ^ " mesh#point");
  if Array1.dim mesh#point_marker > 0
    && Array1.dim mesh#point_marker < NCOLS(point) then
    invalid_arg("dim mesh#point_marker < " ^ COLS ^ " mesh#point");
  let switches = Buffer.create 20 in
  Buffer.add_string switches default_switches;
  (* Check for PSLG *)
  if pslg then (
    if NCOLS(mesh#segment) > 0 then begin
      if NROWS(mesh#segment) <> 2 then invalid_arg(ROWS ^ " segment <> 2");
      if Array1.dim mesh#segment_marker > 0
        && Array1.dim mesh#segment_marker < NCOLS(mesh#segment) then
        invalid_arg("dim mesh#segment_marker < " ^ COLS ^ " mesh#segment");
    end;
    if not refine then begin
      if NCOLS(mesh#hole) > 0 && NROWS(mesh#hole) <> 2 then
        invalid_arg(ROWS ^ " hole <> 2");
      if NCOLS(mesh#region) > 0 && NROWS(mesh#region) <> 4 then
        invalid_arg(ROWS ^ " region <> 4");
    end;
    Buffer.add_char switches 'p';
    if NROWS(mesh#segment) = 0 || NCOLS(mesh#segment) = 0 then
      Buffer.add_char switches 'c';
    (* Check that no point contains NaN.  Triangle seems to go into an
       infinite loop with these which can easily be confused with
       other difficulties.  FIXME: want to do this for any mesh? *)
    for i = FST to NCOLS(point) do
      if is_nan point.{FST, i} then
        invalid_arg(sprintf "mesh#point.{%i, %i} is NaN" FST i);
      if is_nan point.{SND, i} then
        invalid_arg(sprintf "mesh#point.{%i, %i} is NaN" SND i);
    done;
  );
  (* Check for refinement -- triangles *)
  if refine then begin
    if NCOLS(mesh#triangle) > 0 then begin
      if NROWS(mesh#triangle) < 3 then
        invalid_arg(ROWS ^ " mesh#triangle < 3");
      if NROWS(mesh#triangle_attribute) > 0
        && NCOLS(mesh#triangle_attribute) < NCOLS(mesh#triangle) then
        invalid_arg(COLS ^ " mesh#triangle_attribute < "
                    ^ COLS ^ " mesh#triangle");
    end;
    Buffer.add_char switches 'r'
  end;
  (* Check triangle_area *)
  let triangle_area = match triangle_area with
    | None ->
      (match max_area with
      | None -> ()
      | Some a ->
        Buffer.add_char switches 'a';
        Buffer.add_string switches (string_of_float a));
      Array1.create float64 layout 0
    | Some a ->
      if Array1.dim a < NCOLS(mesh#triangle) then
        invalid_arg("dim triangle_area < " ^ COLS ^ " mesh#triangle");
      Buffer.add_char switches 'a';
      a in
  (* Check for a triunsuitable function *)
  (match triunsuitable with
  | None -> ()
  | Some f -> register_triunsuitable f;  Buffer.add_char switches 'u');
  (* Other switches *)
  if delaunay then Buffer.add_char switches 'D';
  (match min_angle with
  | None -> ()
  | Some a ->
    Buffer.add_char switches 'q';
    Buffer.add_string switches (string_of_float a));
  (match max_steiner with
  | None -> ()
  | Some a ->
    Buffer.add_char switches 'S';
    Buffer.add_string switches (string_of_int a));
  if voronoi then Buffer.add_char switches 'v';
  if edge then Buffer.add_char switches 'e';
  if neighbor then Buffer.add_char switches 'n';
  if subparam then Buffer.add_string switches "o2";
  if not debug then Buffer.add_char switches 'Q';
  (* Call triangle and build the resulting objects *)
  let point, point_attribute, point_marker, triangle, triangle_attribute,
    neighbor, segment, segment_marker, edge, edge_marker,
    vor_point, vor_point_attribute, vor_edge, vor_normal =
    triangle (Buffer.contents switches) mesh triangle_area in
  let mesh_out : layout t =
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
