open Bigarray
open Graphics
open Mesh

(* Displaying the mesh with Graphics *)

(* TODO:

   - 'q' to quit
   - allow to zoom in and out
   - display the segments in another color
   - allow to switch on and off the nodes #
*)


let display_LAYOUT ?(width=600) ?(height=600) ?xmin ?xmax ?ymin ?ymax
    ?(xbd=10) ?(ybd=10) ?voronoi ?(segments=true) (mesh: LAYOUT_layout t) =
  (* Compute the natural rectangle around the mesh *)
  let pt = mesh.point in
  let cxmax = ref neg_infinity
  and cxmin = ref infinity
  and cymax = ref neg_infinity
  and cymin = ref infinity in
  for i = BA_FIRST to BA_LASTCOL(pt) do
    let x = BA_GET(pt,BA_FIRST,i) and y = BA_GET(pt,BA_SECOND,i) in
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
    let i0 = BA_GET(mesh.triangle,BA_FIRST,t)
    and i1 = BA_GET(mesh.triangle,BA_SECOND,t)
    and i2 = BA_GET(mesh.triangle,BA_THIRD,t) in
    let x0 = BA_GET(pt,BA_FIRST,i0)
    and y0 = BA_GET(pt,BA_SECOND,i0) in
    let x1 = BA_GET(pt,BA_FIRST,i1)
    and y1 = BA_GET(pt,BA_SECOND,i1) in
    let x2 = BA_GET(pt,BA_FIRST,i2)
    and y2 = BA_GET(pt,BA_SECOND,i2) in
    draw_segment x0 y0 x1 y1;
    draw_segment x1 y1 x2 y2;
    draw_segment x2 y2 x0 y0  in
  (* Drawing itself *)
  open_graph (" " ^ (string_of_int (width + 2 * xbd)) ^ "x"
              ^ (string_of_int (height + 2 * ybd)) ^ "-40+40");
  set_window_title "Mesh";
  (* Triangles and Points *)
  for t = BA_FIRST to BA_LASTCOL(mesh.triangle) do draw_triangle t done;
  for i = BA_FIRST to BA_LASTCOL(pt) do
    draw_pt BA_GET(pt,BA_FIRST,i) BA_GET(pt,BA_SECOND,i)
  done;
  (* Voronoi diagram *)
  begin match voronoi with
  | None -> ()
  | Some vor -> ()
  end;
  try
    while true do
      let status = wait_next_event [Button_down; Key_pressed] in
      if status.keypressed && status.key = 'q' then (
        close_graph();
        raise Exit
      );
    done
  with Exit -> ()
