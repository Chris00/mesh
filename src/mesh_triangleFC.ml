
(* check that all C "triexit" have been avoided. *)

let triangulate ?(delaunay=true) ?min_angle ?max_area
    ?max_steiner ?(voronoi=false) ?(edge=true) ?(neighbor=false)
    ?(subparam=false) ?triangle_area ?triunsuitable
    ?(check_finite=true) ?(debug=true)
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
  if check_finite then (
    (* Check that no point contains NaN (or infinities).  Triangle
       seems to go into an infinite loop with these which can easily
       be confused with other difficulties. *)
    for i = FST to NCOLS(point) do
      if not(is_finite(GET(point, FST, i))) then
        invalid_arg(LINE_COL(sprintf "mesh#point.{%i, %i} is not finite",
                             FST, i));
      if not(is_finite(GET(point, SND, i))) then
        invalid_arg(LINE_COL(sprintf "mesh#point.{%i, %i} is not finite",
                             SND, i));
    done;
  );
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
    if not refine then (
      let hole = mesh#hole in
      if NCOLS(hole) > 0 && NROWS(hole) <> 2 then
        invalid_arg(ROWS ^ " hole <> 2");
      let region = mesh#region in
      if NCOLS(region) > 0 && NROWS(region) <> 4 then
        invalid_arg(ROWS ^ " region <> 4");
      if check_finite then (
        for i = FST to NCOLS(hole) do
          if not(is_finite(GET(hole, FST, i))) then
            invalid_arg(LINE_COL(sprintf "mesh#hole.{%i, %i} is not finite",
                                 FST, i));
          if not(is_finite(GET(hole, SND, i))) then
            invalid_arg(LINE_COL(sprintf "mesh#hole.{%i, %i} is not finite",
                                 SND, i));
        done;
        for i = FST to NCOLS(region) do
          for j = FST to NROWS(region) do
            if not(is_finite(GET(region, j, i))) then
              invalid_arg(LINE_COL(sprintf "mesh#region.{%i, %i} is not finite",
                                   j, i));
          done
        done
      )
    );
    Buffer.add_char switches 'p';
    if NROWS(mesh#segment) = 0 || NCOLS(mesh#segment) = 0 then
      Buffer.add_char switches 'c';
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
    if a < 0. || a > 60. then (* required: 3 min_algle <= 180 *)
      Buffer.add_char switches 'q'
    else
      (* Angle may include a decimal point, but not exponential notation. *)
      bprintf switches "d%f" a);
  (match max_steiner with
   | None -> ()
   | Some a -> bprintf switches "S%i" a);
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
