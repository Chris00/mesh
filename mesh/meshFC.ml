(* Functions for LAYOUT layout.
 ***********************************************************************)

(** Return the smaller box (xmin, xmax, ymin, ymax) containing the [mesh]. *)
let bounding_box (mesh: mesh) =
  let xmin = ref infinity
  and xmax = ref neg_infinity
  and ymin = ref infinity
  and ymax = ref neg_infinity in
  let point = mesh#point in
  for i = FST to LASTCOL(point) do
    let x = GET(point, FST,i)
    and y = GET(point, SND,i) in
    if x > !xmax then xmax := x;
    if x < !xmin then xmin := x;
    if y > !ymax then ymax := y;
    if y < !ymin then ymin := y;
  done;
  (!xmin, !xmax, !ymin, !ymax)

let latex (mesh: mesh) filename =
  let edge = mesh#edge in
  let pt = mesh#point in
  if NCOLS(edge) = 0 then invalid_arg "Mesh.latex: mesh#edge must be nonempty";
  if NROWS(edge) <> 2 then
    invalid_arg "Mesh.latex: mesh#edge must have 2 rows (fortran)";
  if NCOLS(pt) = 0 then invalid_arg "Mesh.latex: mesh#point must be nonempty";
  if NROWS(pt) <> 2 then
    invalid_arg "Mesh.latex: mesh#point must have 2 rows (fortran)";
  let fh = open_out filename in
  let xmin, xmax, ymin, ymax = bounding_box mesh in
  latex_begin fh (xmax -. xmin) (ymax -. ymin) xmin ymin;
  (* Write lines *)
  fprintf fh "  %% %i triangles\n" (NCOLS(mesh#triangle));
  for e = FST to LASTCOL(edge) do
    let i1 = GET(edge, FST,e)
    and i2 = GET(edge, SND,e) in
    let p1 = { x = GET(pt, FST,i1);  y = GET(pt, SND,i1) }
    and p2 = { x = GET(pt, FST,i2);  y = GET(pt, SND,i2) } in
    line fh black p1 p2
  done;
  (* Write points *)
  fprintf fh "  %% %i points\n" (NCOLS(pt));
  for i = FST to LASTCOL(pt) do
    point_xy fh i (GET(pt, FST,i)) (GET(pt, SND,i));
  done;
  latex_end fh;
  close_out fh


let scilab (mesh: mesh) (z: vec) fname =
  let triangle = mesh#triangle in
  let pt = mesh#point in
  if NCOLS(triangle) = 0 then
    invalid_arg "Mesh.scilab: mesh#triangle must be nonempty";
  if NROWS(triangle) < 3 then
    invalid_arg "Mesh.scilab: mesh#triangle must have at least \
	3 rows (fortran)";
  if NCOLS(pt) = 0 then invalid_arg "Mesh.scilab: mesh#point must be nonempty";
  if NROWS(pt) <> 2 then
    invalid_arg "Mesh.scilab: mesh#point must have 2 rows (fortran)";
  let fname =
    if Filename.check_suffix fname ".sci" then Filename.chop_extension fname
    else fname in
  let sci = fname ^ ".sci"
  and xf = fname ^ "_x.dat"
  and yf = fname ^ "_y.dat"
  and zf = fname ^ "_z.dat" in
  let fh = open_out sci in
  fprintf fh "// Run with exec('%s')
xf = fscanfMat('%s');
yf = fscanfMat('%s');
zf = fscanfMat('%s');
clf();
plot3d(xf, yf, zf)\n" sci xf yf zf;
  close_out fh;
  let save_mat fname coord =
    let fh = open_out fname in
    (* We traverse several times the triangles but Scilab will not
       have to transpose the matrices. *)
    for point = FST to THIRD do
      for t = FST to LASTCOL(triangle) do
        fprintf fh "%.13g " (coord (GET(triangle, point,t)))
      done;
      fprintf fh "\n";
    done;
    close_out fh in
  save_mat xf (fun i -> GET(pt, FST,i));
  save_mat yf (fun i -> GET(pt, SND,i));
  save_mat zf (fun i -> z.{i})

let is_allowed c =
  ('0' <= c && c <= '9') || ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c = '_'

let matlab (mesh: mesh) (z: vec) fname =
  let tr = mesh#triangle in
  let pt = mesh#point in
  if NCOLS(tr) = 0 then
    invalid_arg "Mesh.scilab: mesh#triangle must be nonempty";
  if NROWS(tr) < 3 then
    invalid_arg "Mesh.scilab: mesh#triangle must have at least \
	3 rows (fortran)";
  if NCOLS(pt) = 0 then invalid_arg "Mesh.scilab: mesh#point must be nonempty";
  if NROWS(pt) <> 2 then
    invalid_arg "Mesh.scilab: mesh#point must have 2 rows (fortran)";
  let dir = Filename.dirname fname
  and base = Filename.basename fname in
  let base = (if Filename.check_suffix base ".m" then
                 String.sub base 0 (String.length base - 2)
               else String.copy base) in
  (* Matlab filenames can contain only alphanumeric characters and
     underscores.  Convert all other characters to underscore *)
  for i = 0 to String.length base - 1 do
    if not(is_allowed base.[i]) then base.[i] <- '_'
  done;
  let mat = Filename.concat dir (base ^ ".m") in
  let save_xy fh coord =
    for p = FST to LASTCOL(pt) do fprintf fh "%.13g " (GET(pt, coord,p)) done;
    fprintf fh "\n" in
  let fh = open_out mat in
  fprintf fh "%% Created by the OCaml Mesh module (run %s)\nmesh_x = [" mat;
  save_xy fh FST;
  fprintf fh "];\nmesh_y = [";
  save_xy fh SND;
  fprintf fh "];\nmesh_z = [";
  for i = FST to LASTEL(z) do fprintf fh "%.13f " z.{i} done;
  fprintf fh "];\nmesh_triangles = [";
  for t = FST to LASTCOL(tr) do
    fprintf fh "%i %i %i; " (GET(tr, FST,t)) (GET(tr, SND,t)) (GET(tr, THIRD,t))
  done;
  fprintf fh "];\ntrisurf(mesh_triangles, mesh_x, mesh_y, mesh_z);\n";
  close_out fh
;;

DEFINE FUN = "Mesh.level_curves";;
INCLUDE "mesh_level_curvesFC.ml";;

let level_curves ?(boundary=(fun _ -> Some black)) (mesh: mesh) (z: vec)
    ?level_eq levels fname =
  let fh = open_out fname in
  let xmin, xmax, ymin, ymax = bounding_box mesh in
  latex_begin fh (xmax -. xmin) (ymax -. ymin) xmin ymin;
  draw_levels ~boundary mesh z ?level_eq levels fh;
  latex_end fh;
  close_out fh


(* Determine the number of superdiagonals + 1 main diagonal *)
let band_height_P1 (mesh: mesh) =
  let tr = mesh#triangle in
  let kd = ref 0 in
  for i = 1 to Array2.dim2 tr do
    let i1 = tr.{1,i}
    and i2 = tr.{2,i}
    and i3 = tr.{3,i} in
    kd := max4 !kd (abs(i1 - i2)) (abs(i2 -i3)) (abs(i3 - i1))
  done;
  !kd + 1



(* Local Variables: *)
(* compile-command: "make -k" *)
(* End: *)
