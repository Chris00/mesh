
let display ?(width=600) ?(height=600) ?xmin ?xmax ?ymin ?ymax
    ?(xbd=10) ?(ybd=10) ?voronoi ?(segments=true) (mesh: mesh) =
  (* Compute the natural rectangle around the mesh *)
  let pt = mesh.point in
  let cxmax = ref neg_infinity
  and cxmin = ref infinity
  and cymax = ref neg_infinity
  and cymin = ref infinity in
  for i = FST to LASTCOL(pt) do
    let x = GET(pt, FST,i)
    and y = GET(pt, SND,i) in
    if !cxmax < x then cxmax := x;
    if !cxmin > x then cxmin := x;
    if !cymax < y then cymax := y;
    if !cymin > y then cymin := y;
  done;
  (* Set displayed area  *)
  let xmin = match xmin with
    | None -> !cxmin
    | Some x -> x
  and xmax = match xmax with
    | None -> !cxmax
    | Some x -> x
  and ymin = match ymin with
    | None -> !cymin
    | Some x -> x
  and ymax = match ymax with
    | None -> !cymax
    | Some x -> x in
  let hx = float width /. (xmax -. xmin)
  and hy = float height /. (ymax -. ymin) in
  let draw_pt x y =
    fill_circle (truncate((x -. xmin) *. hx) + xbd)
      (truncate((y -. ymin) *. hy) + ybd) 3 in
  let draw_segment x0 y0 x1 y1 =
    let x0 = truncate((x0 -. xmin) *. hx) + xbd
    and y0 = truncate((y0 -. ymin) *. hy) + ybd
    and x1 = truncate((x1 -. xmin) *. hx) + xbd
    and y1 = truncate((y1 -. ymin) *. hy) + ybd in
    draw_segments [| (x0, y0, x1, y1) |] in
  let draw_triangle t =
    let i0 = GET(mesh.triangle, FST,t)
    and i1 = GET(mesh.triangle, SND,t)
    and i2 = GET(mesh.triangle, THIRD,t) in
    let x0 = GET(pt, FST,i0)
    and y0 = GET(pt, SND,i0) in
    let x1 = GET(pt, FST,i1)
    and y1 = GET(pt, SND,i1) in
    let x2 = GET(pt, FST,i2)
    and y2 = GET(pt, SND,i2) in
    draw_segment x0 y0 x1 y1;
    draw_segment x1 y1 x2 y2;
    draw_segment x2 y2 x0 y0  in
  (* Drawing itself *)
  open_graph (" " ^ (string_of_int (width + 2 * xbd)) ^ "x"
              ^ (string_of_int (height + 2 * ybd)) ^ "-40+40");
  set_window_title "Mesh";
  (* Triangles and Points *)
  for t = FST to LASTCOL(mesh.triangle) do draw_triangle t done;
  for i = FST to LASTCOL(pt) do
    draw_pt (GET(pt, FST,i)) (GET(pt, SND,i))
  done;
  (* Voronoi diagram *)
  begin match voronoi with
  | None -> ()
  | Some vor -> ()
  end;
  (* Wait for the key 'q' to be pressed. *)
  try
    while true do
      let status = wait_next_event [Button_down; Key_pressed] in
      if status.keypressed && (status.key = 'q' || status.key = 'Q') then (
        close_graph();
        raise Exit
      );
    done
  with Exit -> ()
