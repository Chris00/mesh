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


let display_fortran ?(width=600) ?(height=600) ?xmin ?xmax ?ymin ?ymax
    ?(xbd=10) ?(ybd=10) ?voronoi ?(segments=true) (mesh: fortran_layout t) =
  (* Compute the natural rectangle around the mesh *)
  let pt = mesh.point in
  let cxmax = ref neg_infinity
  and cxmin = ref infinity
  and cymax = ref neg_infinity
  and cymin = ref infinity in
  for i = 1 to Array2.dim2(pt) do
    let x = pt.{1,i} and y = pt.{2,i} in
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
    let i0 = mesh.triangle.{1,t}
    and i1 = mesh.triangle.{2,t}
    and i2 = mesh.triangle.{3,t} in
    let x0 = pt.{1,i0}
    and y0 = pt.{2,i0} in
    let x1 = pt.{1,i1}
    and y1 = pt.{2,i1} in
    let x2 = pt.{1,i2}
    and y2 = pt.{2,i2} in
    draw_segment x0 y0 x1 y1;
    draw_segment x1 y1 x2 y2;
    draw_segment x2 y2 x0 y0  in
  (* Drawing itself *)
  open_graph (" " ^ (string_of_int (width + 2 * xbd)) ^ "x"
              ^ (string_of_int (height + 2 * ybd)) ^ "-40+40");
  set_window_title "Mesh";
  (* Triangles and Points *)
  for t = 1 to Array2.dim2(mesh.triangle) do draw_triangle t done;
  for i = 1 to Array2.dim2(pt) do
    draw_pt pt.{1,i} pt.{2,i}
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


let display_c ?(width=600) ?(height=600) ?xmin ?xmax ?ymin ?ymax
    ?(xbd=10) ?(ybd=10) ?voronoi ?(segments=true) (mesh: c_layout t) =
  (* Compute the natural rectangle around the mesh *)
  let pt = mesh.point in
  let cxmax = ref neg_infinity
  and cxmin = ref infinity
  and cymax = ref neg_infinity
  and cymin = ref infinity in
  for i = 0 to Array2.dim1(pt) - 1 do
    let x = pt.{i,0} and y = pt.{i,1} in
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
    let i0 = mesh.triangle.{t,0}
    and i1 = mesh.triangle.{t,1}
    and i2 = mesh.triangle.{t,2} in
    let x0 = pt.{i0,0}
    and y0 = pt.{i0,1} in
    let x1 = pt.{i1,0}
    and y1 = pt.{i1,1} in
    let x2 = pt.{i2,0}
    and y2 = pt.{i2,1} in
    draw_segment x0 y0 x1 y1;
    draw_segment x1 y1 x2 y2;
    draw_segment x2 y2 x0 y0  in
  (* Drawing itself *)
  open_graph (" " ^ (string_of_int (width + 2 * xbd)) ^ "x"
              ^ (string_of_int (height + 2 * ybd)) ^ "-40+40");
  set_window_title "Mesh";
  (* Triangles and Points *)
  for t = 0 to Array2.dim1(mesh.triangle) - 1 do draw_triangle t done;
  for i = 0 to Array2.dim1(pt) - 1 do
    draw_pt pt.{i,0} pt.{i,1}
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
(* Displaying meshes -- gathering the FORTRAN and C layouts together
 ***********************************************************************)

let display ?width ?height ?xmin ?xmax ?ymin ?ymax
    ?xbd ?ybd ?voronoi ?segments (mesh: 'a t) =
  if Mesh.is_c_layout mesh then
    Obj.magic (display_c ?width ?height ?xmin ?xmax ?ymin ?ymax
                 ?xbd ?ybd ?voronoi ?segments (Obj.magic mesh))
  else
    Obj.magic (display_fortran ?width ?height ?xmin ?xmax ?ymin ?ymax
                 ?xbd ?ybd ?voronoi ?segments (Obj.magic mesh))
