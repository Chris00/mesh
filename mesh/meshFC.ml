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

let latex ?edge:(edge_color=fun _ -> Some black) (mesh: mesh) filename =
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
    match edge_color e with
    | None -> ()
    | Some color ->
      let i1 = GET(edge, FST,e)
      and i2 = GET(edge, SND,e) in
      let p1 = { x = GET(pt, FST,i1);  y = GET(pt, SND,i1) }
      and p2 = { x = GET(pt, FST,i2);  y = GET(pt, SND,i2) } in
      line fh color p1 p2
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
    invalid_arg "Mesh.matlab: mesh#triangle must be nonempty";
  if NROWS(tr) < 3 then
    invalid_arg "Mesh.matlab: mesh#triangle must have at least \
	3 rows (fortran)";
  if NCOLS(pt) = 0 then invalid_arg "Mesh.matlab: mesh#point must be nonempty";
  if NROWS(pt) <> 2 then
    invalid_arg "Mesh.matlab: mesh#point must have 2 rows (fortran)";
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

(* Return an array [adj] such that [adj.(i)] is the list of the
   adjacent nodes to [i]. *)
let adjacency (mesh: mesh) =
  let n = NCOLS(mesh#point) in
  let adj = Array.make (n + FST) [] in
  let edge = mesh#edge in
  for e = FST to LASTCOL(edge) do
    let i1 = GET(edge, FST, e)
    and i2 = GET(edge, SND, e) in
    adj.(i1) <- i2 :: adj.(i1);
    adj.(i2) <- i1 :: adj.(i2);
  done;
  adj

let mathematica (mesh: mesh) (z: vec) fname =
  let pt = mesh#point in
  if NCOLS(pt) = 0 then
    invalid_arg "Mesh.mathematica: mesh#point must be nonempty";
  if NROWS(pt) <> 2 then
    invalid_arg "Mesh.mathematica: mesh#point must have 2 rows (fortran)";
  if NCOLS(mesh#edge) = 0 then
    invalid_arg "Mesh.mathematica: mesh#edge must be nonempty";
  if NROWS(mesh#edge) <> 2 then
    invalid_arg "Mesh.mathematica: mesh#edge must have 2 rows (fortran)";
  let dir = Filename.dirname fname in
  let base = Filename.basename fname in
  let base, fname =
    if Filename.check_suffix base ".m" then
      String.sub base 0 (String.length base - 2), fname
    else String.copy base, fname ^ ".m" in
  (* Convert all chars that are not alphanumeric ot '_' to '_'. *)
  for i = 0 to String.length base - 1 do
    if not(is_allowed base.[i]) then base.[i] <- '_'
  done;
  let pkg = String.capitalize base in
  let fh = open_out (Filename.concat dir (base ^ ".m")) in
  fprintf fh "(* Created by the OCaml Mesh module *)\n";
  fprintf fh "BeginPackage[\"%s`\"];\nBegin[\"`Private`\"];\n" pkg;
  fprintf fh "xyz = {";
  fprintf fh "{%.16g, %.16g, %.16g}" pt.{FST, FST} pt.{SND, FST} z.{FST};
  for i = FST + 1 to LASTCOL(pt) do
    fprintf fh ", {%.16g, %.16g, %.16g}" pt.{FST, i} pt.{SND, i} z.{i}
  done;
  fprintf fh "};\n\n";
  let adj = adjacency mesh in
  let output_adj i =
    (* mathematica indices start at 1 *)
    match adj.(i) with
    | [] -> fprintf fh "{%i, {}}" (TO_FORTRAN(i))
    | n :: tl ->
      fprintf fh "{%i, {%i" (TO_FORTRAN(i)) (TO_FORTRAN(n));
      List.iter (fun n -> fprintf fh ", %i" (TO_FORTRAN(n))) tl;
      fprintf fh"}}" in
  fprintf fh "mesh = {";
  output_adj FST;
  for i = FST + 1 to Array.length adj - 1 do
    output_string fh ", "; output_adj i
  done;
  fprintf fh "};\n\n";
  fprintf fh "Needs[\"ComputationalGeometry`\"];\n";
  fprintf fh "TriangularSurfacePlot[xyz, mesh];\n";
  fprintf fh "End[ ];\nEndPackage[ ];\n";
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
  for t = FST to LASTCOL(tr) do
    let i1 = GET(tr, FST, t)
    and i2 = GET(tr, SND, t)
    and i3 = GET(tr, THIRD, t) in
    kd := max4 !kd (abs(i1 - i2)) (abs(i2 -i3)) (abs(i3 - i1))
  done;
  !kd + 1

(* Return the index with the lowest nonnegative [deg] (negative
   degrees are ignored).  Return [-1] if all degrees are < 0. *)
let min_deg (deg: int array) =
  let i = ref(-1) in
  let degi = ref(max_int) in
  for j = FST to Array.length deg - 1 do
    if deg.(j) >= 0 && deg.(j) < !degi then (i := j;  degi := deg.(j))
  done;
  !i

(** Apply the permutation [perm] to the [mesh]. *)
let permute perm (mesh: mesh) : mesh =
  let n = NCOLS(mesh#point) in
  (* Inverse perm *)
  let inv_perm = Array1.create int layout n in
  for i = FST to LASTEL(perm) do inv_perm.{perm.{i}} <- i done;
  (* Build the new mesh *)
  let old_pt = mesh#point in
  let pt = ARRAY2(float64, 2, n) in
  for i = FST to LASTCOL(pt) do
    let old_i = perm.{i} in
    GET(pt, FST, i) <- GET(old_pt, FST, old_i);
    GET(pt, SND, i) <- GET(old_pt, SND, old_i);
  done;
  let old_ptm = mesh#point_marker in
  let ptm = Array1.create int layout (Array1.dim old_ptm) in
  for i = FST to LASTEL(ptm) do ptm.{i} <- old_ptm.{perm.{i}} done;
  let old_seg = mesh#segment in
  let seg = ARRAY2(int, 2, NCOLS(old_seg)) in
  for s = FST to LASTCOL(seg) do
    GET(seg, FST, s) <- inv_perm.{GET(old_seg, FST, s)};
    GET(seg, SND, s) <- inv_perm.{GET(old_seg, SND, s)};
  done;
  let old_tr = mesh#triangle in
  let tr = ARRAY2(int, NROWS(old_tr), NCOLS(old_tr)) in
  for t = FST to LASTCOL(tr) do
    for c = FST to LASTROW(tr) do
      GET(tr, c, t) <- inv_perm.{GET(old_tr, c, t)}
    done;
  done;
  let old_edge = mesh#edge in
  let edge = ARRAY2(int, 2, NCOLS(old_edge)) in
  for e = FST to LASTCOL(edge) do
    GET(edge, FST, e) <- inv_perm.{GET(old_edge, FST, e)};
    GET(edge, SND, e) <- inv_perm.{GET(old_edge, SND, e)};
  done;
  object
    method point = pt
    method point_marker = ptm
    method segment = seg
    method segment_marker = mesh#segment_marker
    method hole = mesh#hole
    method region = mesh#region
    method triangle = tr
    method neighbor = mesh#neighbor
    method edge = edge
    method edge_marker = mesh#edge_marker
  end


(* http://ciprian-zavoianu.blogspot.com/2009/01/project-bandwidth-reduction.html
*)
let cuthill_mckee ~rev perm (mesh: mesh) : mesh =
  let n = NCOLS(mesh#point) in
  let perm = match perm with
    | None -> Array1.create int layout n
    | Some p ->
      if Array1.dim p <> n then
        invalid_arg "Mesh.cuthill_mckee: dim perm <> number of points";
      p in
  let deg = Array.make (n + FST) 0 in (* degree of adjacency of each node *)
  let nbh = Array.make (n + FST) [] in (* list of adjacent nodes *)
  let edge = mesh#edge in
  for e = FST to LASTCOL(edge) do
    let i1 = GET(edge, FST, e)
    and i2 = GET(edge, SND, e) in
    nbh.(i1) <- i2 :: nbh.(i1);
    deg.(i1) <- deg.(i1) + 1;
    nbh.(i2) <- i1 :: nbh.(i2);
    deg.(i2) <- deg.(i2) + 1;
  done;
  let free = ref(FST) in (* first free position in [perm] *)
  let q = Queue.create () in
  let add node =
    perm.{!free} <- node;
    incr free;
    deg.(node) <- -1; (* [i] put in the final vec. *)
    let nbhs = List.filter (fun i -> deg.(i) >= 0) nbh.(node) in
    let nbhs = List.fast_sort (fun i1 i2 -> compare deg.(i1) deg.(i2)) nbhs in
    List.iter (fun i -> Queue.add i q) nbhs
  in
  let last_pt = LASTEL(perm) in
  while !free <= last_pt do
    add (min_deg deg);
    while not(Queue.is_empty q) do
      let c = Queue.take q in
      if deg.(c) >= 0 then add c
    done
  done;
  if rev then (
    let s = if FST = 0 then n-1 else n+1 in (* FIXME: cond known at compil. *)
    for i = FST to n/2 -1 + FST do
      let t = perm.{i} in
      perm.{i} <- perm.{s-i};
      perm.{s-i} <- t;
    done
  );
  permute perm mesh

(* A Generalized GPS Algorithm For Reducing The Bandwidth And Profile
   Of A Sparse Matrix, Q. Wang, Y. C. Guo, and X. W. Shi
   http://www.jpier.org/PIER/pier90/09.09010512.pdf *)
let ggps perm (mesh: mesh) : mesh =
  let n = NCOLS(mesh#point) in
  let perm = match perm with
    | None -> Array1.create int layout n
    | Some p ->
      if Array1.dim p <> n then
        invalid_arg "Mesh.ggps: dim perm <> number of points";
      p in
  let deg = Array.make (n + FST) 0 in (* degree of adjacency of each node *)
  let edge = mesh#edge in
  for e = FST to LASTCOL(edge) do
    let i1 = GET(edge, FST, e)
    and i2 = GET(edge, SND, e) in
    deg.(i1) <- deg.(i1) + 1;
    deg.(i2) <- deg.(i2) + 1;
  done;
  let v = min_deg deg in

  permute perm mesh

(* Local Variables: *)
(* compile-command: "make -k" *)
(* End: *)
