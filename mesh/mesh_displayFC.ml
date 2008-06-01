(** Return the smaller box (xmin, xmax, ymin, ymax) containing the [mesh]. *)
let bounding_box (mesh: mesh) =
  let pt = mesh.point in
  let xmin = ref infinity
  and xmax = ref neg_infinity
  and ymin = ref infinity
  and ymax = ref neg_infinity in
  for i = FST to LASTCOL(pt) do
    let x = GET(pt, FST,i)
    and y = GET(pt, SND,i) in
    if x > !xmax then xmax := x;
    if x < !xmin then xmin := x;
    if y > !ymax then ymax := y;
    if y < !ymin then ymin := y;
  done;
  (!xmin, !xmax, !ymin, !ymax)

let draw ?(width=600) ?(height=600) ?(color=foreground) ?voronoi
    ?(segments=true) ?point_marker_color (mesh: mesh) =
  let xmin, xmax, ymin, ymax = bounding_box mesh in
  let hx = float width /. (xmax -. xmin)
  and hy = float height /. (ymax -. ymin) in
  let (xbd, ybd) = current_point() in
  let draw_pt x y =
    fill_circle (truncate((x -. xmin) *. hx) + xbd)
      (truncate((y -. ymin) *. hy) + ybd) 3 in
  let draw_segment x0 y0 x1 y1 =
    let x0 = truncate((x0 -. xmin) *. hx) + xbd
    and y0 = truncate((y0 -. ymin) *. hy) + ybd
    and x1 = truncate((x1 -. xmin) *. hx) + xbd
    and y1 = truncate((y1 -. ymin) *. hy) + ybd in
    draw_segments [| (x0, y0, x1, y1) |] in
  (* Triangles and Points *)
  let pt = mesh.point
  and triangle = mesh.triangle in
  set_color color;
  for t = FST to LASTCOL(triangle) do
    (* Draw triangle [t]. *)
    let i0 = GET(triangle, FST,t)
    and i1 = GET(triangle, SND,t)
    and i2 = GET(triangle, THIRD,t) in
    try
      let x0 = GET(pt, FST,i0)
      and y0 = GET(pt, SND,i0) in
      let x1 = GET(pt, FST,i1)
      and y1 = GET(pt, SND,i1) in
      let x2 = GET(pt, FST,i2)
      and y2 = GET(pt, SND,i2) in
      draw_segment x0 y0 x1 y1;
      draw_segment x1 y1 x2 y2;
      draw_segment x2 y2 x0 y0;
    with e ->
      eprintf "mesh_display: triangle %i (%i,%i,%i): %s\n%!"
        t i0 i1 i2 (Printexc.to_string e)
  done;
  let marker = match point_marker_color with
    | None -> (fun _ _ _ -> ())
    | Some c -> (fun m x y ->
                   set_color c;
                   let px = truncate((x -. xmin) *. hx) + xbd
                   and py = truncate((y -. ymin) *. hy) + ybd in
                   moveto px py;
                   draw_string(string_of_int m);
                   set_color color
                ) in
  for i = FST to LASTCOL(pt) do
    let x = GET(pt, FST,i)  and y = GET(pt, SND,i) in
    draw_pt x y;
    marker mesh.point_marker.{i} x y
  done;
  (* Voronoi diagram *)
  begin match voronoi with
  | None -> ()
  | Some vor -> ()                      (* FIXME: todo *)
  end
