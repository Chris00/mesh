(** Return the smaller box (xmin, xmax, ymin, ymax) containing the [mesh]. *)
let bounding_box (mesh: mesh) =
  let pt = mesh#point in
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

(** Contains the information to transform "mesh coordinates" into the
    graphics ones. *)
type surf = { hx: float; hy: float;  xbd: int;  ybd: int;
              xmin: float;  ymin: float }

let draw_pt s x y =
  fill_circle (truncate((x -. s.xmin) *. s.hx) + s.xbd)
    (truncate((y -. s.ymin) *. s.hy) + s.ybd) 3

let draw_segment s x0 y0 x1 y1 =
  let x0 = truncate((x0 -. s.xmin) *. s.hx) + s.xbd
  and y0 = truncate((y0 -. s.ymin) *. s.hy) + s.ybd
  and x1 = truncate((x1 -. s.xmin) *. s.hx) + s.xbd
  and y1 = truncate((y1 -. s.ymin) *. s.hy) + s.ybd in
  draw_segments [| (x0, y0, x1, y1) |]

let make_surf mesh width height =
  let xmin, xmax, ymin, ymax = bounding_box mesh in
  let hx = float width /. (xmax -. xmin)
  and hy = float height /. (ymax -. ymin) in
  let (xbd, ybd) = current_point() in
  { hx = hx; hy = hy;  xbd = xbd; ybd = ybd;  xmin = xmin; ymin = ymin }

let draw ?(width=600) ?(height=600) ?(color=foreground) ?voronoi
    ?(segments=true) ?point_marker_color (mesh: mesh) =
  let surf = make_surf mesh width height in
  (* Triangles and Points *)
  let pt = mesh#point
  and triangle = mesh#triangle in
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
      draw_segment surf x0 y0 x1 y1;
      draw_segment surf x1 y1 x2 y2;
      draw_segment surf x2 y2 x0 y0;
    with e ->
      eprintf "mesh_display: triangle %i (%i,%i,%i): %s\n%!"
        t i0 i1 i2 (Printexc.to_string e)
  done;
  let marker = match point_marker_color with
    | None -> (fun _ _ _ -> ())
    | Some c -> (fun m x y ->
                  set_color c;
                  let px = truncate((x -. surf.xmin) *. surf.hx) + surf.xbd
                  and py = truncate((y -. surf.ymin) *. surf.hy) + surf.ybd in
                  moveto px py;
                  draw_string(string_of_int m);
                  set_color color
               ) in
  let pt_marker = mesh#point_marker in
  for i = FST to LASTCOL(pt) do
    let x = GET(pt, FST,i)  and y = GET(pt, SND,i) in
    draw_pt surf x y;
    marker pt_marker.{i} x y
  done;
  (* Voronoi diagram *)
  begin match voronoi with
  | None -> ()
  | Some vor -> ()                      (* FIXME: todo *)
  end
;;

type point = { x : float; y : float }

let point s i {x=x; y=y} = draw_pt s x y

let line s color {x=x0; y=y0} {x=x1; y=y1} =
  set_color color;
  draw_segment s x0 y0 x1 y1

let triangle s color {x=x0; y=y0} {x=x1; y=y1} {x=x2; y=y2} =
  let x0 = truncate((x0 -. s.xmin) *. s.hx) + s.xbd
  and y0 = truncate((y0 -. s.ymin) *. s.hy) + s.ybd
  and x1 = truncate((x1 -. s.xmin) *. s.hx) + s.xbd
  and y1 = truncate((y1 -. s.ymin) *. s.hy) + s.ybd
  and x2 = truncate((x2 -. s.xmin) *. s.hx) + s.xbd
  and y2 = truncate((y2 -. s.ymin) *. s.hy) + s.ybd in
  set_color color;
  fill_poly [| (x0, y0); (x1, y1); (x2, y2) |]

DEFINE FUN = "Mesh_display.level_curves"
INCLUDE "mesh_level_curvesFC.ml";;

let level_curves ~width ~height ?(boundary=(fun _ -> Some 0))
    (mesh: mesh) (z: vec) ?level_eq levels =
  let surf = make_surf mesh width height in
  draw_levels ~boundary mesh z ?level_eq levels surf
