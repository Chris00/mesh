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


(* Sort the vertices at node [n0] by increasing (counterclockwise)
   angle w.r.t. the base vertex [i0].  [TriangularSurfacePlot] (not
   [PlanarGraphPlot] it seems) requires the vertices to be ordered. *)
let sort_counterclockwise (pt: matrix) n0 = function
  | ([] | [_]) as adj -> adj
  | n1 :: tl ->
    let x0 = pt.{FST, n0} and y0 = pt.{SND, n0} in
    let dx1 = pt.{FST, n1} -. x0 and dy1 = pt.{SND, n1} -. y0 in
    (* Since [atan2] returns an angle in ]-pi, pi], the angle of
       (dx1,dy1) will be set to pi so that the order given by the
       angles is correct.  Also there is no need to norm the vectors
       [(dx1,dy1)] and [(dx,dy)] because that will only dilate
       [(e1,e2)] which does not change the value of [atan2]. *)
    let angle n =
      let dx = pt.{FST, n} -. x0 and dy = pt.{SND, n} -. y0 in
      let e1 = -. dx *. dx1 -. dy *. dy1
      and e2 = dx *. dy1 -. dy *. dx1 in
      atan2 e2 e1 in
    (* Add angles *)
    let tl = List.map (fun n -> (n, angle n)) tl in
    let tl = List.fast_sort (fun (_,a1) (_,a2) -> compare a1 a2) tl in
    n1 :: List.map (fun (n,_) -> n) tl
;;

(* Return an array [adj] such that [adj.(i)] is the list of the
   adjacent nodes to [i]. *)
let adjacency (mesh: mesh) =
  let pt = mesh#point in
  let n = NCOLS(pt) in
  let adj = Array.make (n + FST) [] in
  let edge = mesh#edge in
  for e = FST to LASTCOL(edge) do
    let i1 = GET(edge, FST, e)
    and i2 = GET(edge, SND, e) in
    adj.(i1) <- i2 :: adj.(i1);
    adj.(i2) <- i1 :: adj.(i2);
  done;
  (* This is important for TriangularSurfacePlot (that uses the order
     for orientation?).  *)
  Array.mapi (fun n0 adj -> sort_counterclockwise pt n0 adj) adj

let is_allowed_mathematica c =
  ('0' <= c && c <= '9') || ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

(* Remove all chars that are not alphanumeric. *)
let mathematica_safe base =
  let j = ref 0 in
  for i = 0 to String.length base - 1 do
    if is_allowed_mathematica base.[i] then (
      base.[!j] <- base.[i];
      incr j;
    )
  done;
  if !j < String.length base then String.sub base 0 !j else base

let mathematica_print_float fh f =
  let s = sprintf "%.16g" f in
  try
    let e = String.index s 'e' in
    output fh s 0 e;  output_string fh "*^";
    output fh s (e + 1) (String.length s - e - 1)
  with Not_found ->
    output fh s 0 (String.length s)

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
  let base = Filename.basename fname in
  let pkg, fname =
    if Filename.check_suffix base ".m" then
      mathematica_safe(String.sub base 0 (String.length base - 2)), fname
    else mathematica_safe base, fname ^ ".m" in
  let pkg = String.capitalize pkg in
  let fh = open_out fname in
  fprintf fh "(* Created by the OCaml Mesh module *)\n";
  fprintf fh "%s`xyz = {" pkg;
  output_string fh "{";
  mathematica_print_float fh pt.{FST, FST};  output_string fh ", ";
  mathematica_print_float fh pt.{SND, FST};  output_string fh ", ";
  mathematica_print_float fh z.{FST};        output_string fh "}";
  for i = FST + 1 to LASTCOL(pt) do
    output_string fh ", {";
    mathematica_print_float fh pt.{FST, i};  output_string fh ", ";
    mathematica_print_float fh pt.{SND, i};  output_string fh ", ";
    mathematica_print_float fh z.{i};        output_string fh "}"
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
  fprintf fh "%s`adj = {" pkg;
  output_adj FST;
  for i = FST + 1 to Array.length adj - 1 do
    output_string fh ", "; output_adj i
  done;
  fprintf fh "};\n\n";
  fprintf fh "Needs[\"ComputationalGeometry`\"];\n";
  fprintf fh "TriangularSurfacePlot[%s`xyz, %s`adj, Axes -> True]\n" pkg pkg;
  close_out fh
;;


DEFINE MOD = "Mesh";;
INCLUDE "mesh_level_curvesFC.ml";;

let level_curves ?(boundary=(fun _ -> Some black)) (mesh: mesh) (z: vec)
    ?level_eq levels fname =
  let fh = open_out fname in
  let xmin, xmax, ymin, ymax = bounding_box mesh in
  latex_begin fh (xmax -. xmin) (ymax -. ymin) xmin ymin;
  draw_levels ~boundary mesh z ?level_eq levels fh;
  latex_end fh;
  close_out fh

let super_level ?boundary (mesh: mesh) (z: vec) level color fname =
  let fh = open_out fname in
  let xmin, xmax, ymin, ymax = bounding_box mesh in
  latex_begin fh (xmax -. xmin) (ymax -. ymin) xmin ymin;
  draw_super_level ?boundary mesh z level color fh;
  latex_end fh;
  close_out fh

let sub_level ?boundary (mesh: mesh) (z: vec) level color fname =
  let fh = open_out fname in
  let xmin, xmax, ymin, ymax = bounding_box mesh in
  latex_begin fh (xmax -. xmin) (ymax -. ymin) xmin ymin;
  draw_sub_level ?boundary mesh z level color fh;
  latex_end fh;
  close_out fh


(* Determine the number of superdiagonals + 1 main diagonal *)
let band_height_P1 filter (mesh: mesh) =
  let tr = mesh#triangle in
  let kd = ref 0 in
  match filter with
  | None ->
    for t = FST to LASTCOL(tr) do
      let i1 = GET(tr, FST, t)
      and i2 = GET(tr, SND, t)
      and i3 = GET(tr, THIRD, t) in
      kd := max4 !kd (abs(i1 - i2)) (abs(i2 -i3)) (abs(i3 - i1))
    done;
    !kd + 1
  | Some cond ->
    for t = FST to LASTCOL(tr) do
      let i1 = GET(tr, FST, t)
      and i2 = GET(tr, SND, t)
      and i3 = GET(tr, THIRD, t) in
      if cond i1 then (
        if cond i2 then
          if cond i3 then
            kd := max4 !kd (abs(i1 - i2)) (abs(i2 -i3)) (abs(i3 - i1))
          else (* exlude i3 *)
            kd := max2 !kd (abs(i2 - i1))
        else (* exclude i2 *) if cond i3 then
          kd := max2 !kd (abs(i3 - i1))
      )
      else (* exclude i1 *) if cond i2 && cond i3 then
        kd := max2 !kd (abs(i3 - i2))
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
let do_permute (perm: int_vec) (inv_perm: int_vec) (mesh: mesh) n : mesh =
  (* Build the new mesh *)
  let old_pt = mesh#point in
  let pt = ARRAY2(float64, 2, n) in
  let last_pt_idx = LASTCOL(pt) in
  for i = FST to last_pt_idx do
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
    let i1 = GET(old_seg, FST, s) in
    if i1 < FST || i1 > last_pt_idx then
      failwith(sprintf "Mesh.permute_points: mesh#segment.{%i} = %i not in \
                        [%i..%i]" s i1 FST last_pt_idx);
    GET(seg, FST, s) <- inv_perm.{i1};
    let i2 = GET(old_seg, SND, s) in
    if i2 < FST || i2 > last_pt_idx then
      failwith(sprintf "Mesh.permute_points: mesh#segment.{%i} = %i not in \
                        [%i..%i]" s i2 FST last_pt_idx);
    GET(seg, SND, s) <- inv_perm.{i2};
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

let permute_unsafe perm mesh =
  let n = NCOLS(mesh#point) in
  (* Inverse perm *)
  let inv_perm = Array1.create int layout n in
  for i = FST to LASTEL(perm) do inv_perm.{perm.{i}} <- i done;
  do_permute perm inv_perm mesh n

let permute_points ~inv perm mesh =
  let n = NCOLS(mesh#point) in
  (* Inverse perm and check that [perm] is indeed a permuation. *)
  let inv_perm = Array1.create int layout n in
  Array1.fill inv_perm (-1); (* never an index *)
  let last_el = LASTEL(perm) in
  for i = FST to last_el do
    let pi = perm.{i} in
    if pi < FST || pi > last_el then
      invalid_arg(sprintf "Mesh.permute_points: perm.{%i} = %i not in [%i..%i]"
                    i pi FST last_el)
    else if inv_perm.{pi} < 0 then inv_perm.{pi} <- i
    else invalid_arg(sprintf "Mesh.permute_points: not a permutation \
      (perm.{%i} = %i = perm.{%i})" inv_perm.{pi} pi i)
  done;
  if inv then do_permute inv_perm perm mesh n
  else do_permute perm inv_perm mesh n


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
  permute_unsafe perm mesh

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

  permute_unsafe perm mesh

(* Local Variables: *)
(* compile-command: "make -k" *)
(* End: *)
