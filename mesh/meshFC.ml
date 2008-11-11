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
  let fh = open_out filename in
  let xmin, xmax, ymin, ymax = bounding_box mesh in
  latex_begin fh (xmax -. xmin) (ymax -. ymin) xmin ymin;
  (* Write lines *)
  fprintf fh "  %% %i triangles\n" (NCOLS(mesh#triangle));
  let edge = mesh#edge in
  let pt = mesh#point in
  for e = FST to LASTCOL(edge) do
    let i1 = GET(edge, FST,e)
    and i2 = GET(edge, SND,e) in
    let p1 = { x = GET(pt, FST,i1);  y = GET(pt, SND,i1) }
    and p2 = { x = GET(pt, FST,i2);  y = GET(pt, SND,i2) } in
    line fh "black" p1 p2
  done;
  (* Write points *)
  fprintf fh "  %% %i points\n" (NCOLS(pt));
  for i = FST to LASTCOL(pt) do
    point_xy fh i (GET(pt, FST,i)) (GET(pt, SND,i));
  done;
  latex_end fh;
  close_out fh


let scilab (mesh: mesh) (z: vec) fname =
  let fname = try Filename.chop_extension fname with _ -> fname in
  let sci = fname ^ ".sci"
  and xf = fname ^ "_x.dat"
  and yf = fname ^ "_y.dat"
  and zf = fname ^ "_z.dat" in
  let fh = open_out sci in
  fprintf fh "// Run with exec('%s')
xf = fscanfMat('%s');
yf = fscanfMat('%s');
zf = fscanfMat('%s');
xbasc();
plot3d(xf, yf, zf)\n" sci xf yf zf;
  close_out fh;
  let save_mat fname coord =
    let fh = open_out fname in
    (* We traverse several times the triangles but Scilab will not
       have to transpose the matrices. *)
    let triangle = mesh#triangle in
    for point = FST to THIRD do
      for t = FST to LASTCOL(triangle) do
        fprintf fh "%.13g " (coord (GET(triangle, point,t)))
      done;
      fprintf fh "\n";
    done;
    close_out fh in
  let pt = mesh#point in
  save_mat xf (fun i -> GET(pt, FST,i));
  save_mat yf (fun i -> GET(pt, SND,i));
  save_mat zf (fun i -> z.{i})

let matlab (mesh: mesh) (z: vec) fname =
  let fname = try Filename.chop_extension fname with _ -> fname in
  let pt = mesh#point in
  let save_xy fh coord =
    for p = FST to LASTCOL(pt) do fprintf fh "%.13g " (GET(pt, coord,p)) done;
    fprintf fh "\n" in
  let mat = fname ^ ".m" in
  let fh = open_out mat in
  fprintf fh "%% Created by the OCaml Mesh module (run %s)\nmesh_x = [" mat;
  save_xy fh FST;
  fprintf fh "];\nmesh_y = [";
  save_xy fh SND;
  fprintf fh "];\nmesh_z = [";
  for i = FST to LASTEL(z) do fprintf fh "%.13f " z.{i} done;
  fprintf fh "];\nmesh_triangles = [";
  let tr = mesh#triangle in
  for t = FST to LASTCOL(tr) do
    fprintf fh "%i %i %i; " (GET(tr, FST,t)) (GET(tr, SND,t)) (GET(tr, THIRD,t))
    done;
  fprintf fh "];\ntrisurf(mesh_triangles, mesh_x, mesh_y, mesh_z);\n";
  close_out fh
;;


let level_curves ?(boundary=(fun _ -> Some "black")) (mesh: mesh) (z: vec)
    ?(level_eq=level_eq_default) levels fname =
  let fh = open_out fname in
  let xmin, xmax, ymin, ymax = bounding_box mesh in
  latex_begin fh (xmax -. xmin) (ymax -. ymin) xmin ymin;
  let bd = Edge.make() in
  (* Draw the boundary edges *)
  let edge = mesh#edge in
  let marker = mesh#edge_marker in
  let pt = mesh#point in
  for e = FST to LASTCOL(edge) do
    let m = marker.{e} in
    if m <> 0 (* not an interior point *) then begin
      let i1 = GET(edge, FST,e)
      and i2 = GET(edge, SND,e) in
      Edge.add bd i1 i2; (* collect boundary points *)
      match boundary m with
      | None -> ()
      | Some color ->
          let p1 = { x = GET(pt, FST,i1);  y = GET(pt, SND,i1) }
          and p2 = { x = GET(pt, FST,i2);  y = GET(pt, SND,i2) } in
          line fh color p1 p2
    end
  done;
  let tr = mesh#triangle in
  let marker = mesh#point_marker in
  for t = FST to LASTCOL(tr) do
    let i1 = GET(tr, FST,t)
    and i2 = GET(tr, SND,t)
    and i3 = GET(tr, THIRD,t) in
    let p1 = { x = GET(pt, FST,i1);  y = GET(pt, SND,i1) }
    and z1 = z.{i1} in
    let p2 = { x = GET(pt, FST,i2);  y = GET(pt, SND,i2) }
    and z2 = z.{i2} in
    let p3 = { x = GET(pt, FST,i3);  y = GET(pt, SND,i3) }
    and z3 = z.{i3} in
    List.iter
      (fun (l, color) ->
         (* Draw the level curve [l] on the triangle [t] except if
            that curve is on the boundary. *)
         if level_eq l z1 then (
           if level_eq l z2 then (
             if level_eq l z3 then
               (* The entire triangle is at the same level.  Try to
                  remove boundary edges. *)
               if Edge.on bd i1 i2 then
                 if Edge.on bd i1 i3 || Edge.on bd i2 i3 then
                   triangle fh color p1 p2 p3 (* Full triangle ! *)
                 else line fh color p3 (mid p1 p2)
               else (* i1-i2 not on boundary *)
                 if Edge.on bd i1 i3 then
                   if Edge.on bd i2 i3 then triangle fh color p1 p2 p3
                   else line fh color p2 (mid p1 p3)
                 else (* i1-i3 not on boundary *)
                   if Edge.on bd i2 i3 then line fh color p1 (mid p2 p3)
                   else triangle fh color p1 p2 p3 (* Full triangle ! *)
             else (* l = z1 = z2 <> z3 *)
               if not(Edge.on bd i1 i2) then line fh color p1 p2
           )
           else (* l = z1 <> z2, z3 *)
             if (z2 < l && l < z3) || (z3 < l && l < z2) then
               line fh color p1 (intercept p2 z2 p3 z3 l)
         )
         else if l < z1 then (
           if level_eq l z2 then
             if level_eq l z3 then
               (if not(Edge.on bd i2 i3) then line fh color p2 p3)
             else if l > z3 then (* z3 < l = z2 < z1 *)
               line fh color p2 (intercept p1 z1 p3 z3 l)
             else (* isolated point, inside the domain *)
               (if marker.{i2} = 0 then point fh i2 p2)
           else if l < z2 then (
             if level_eq l z3 then
               (if marker.{i3} = 0 then point fh i3 p3)
             else if l > z3 then
               line fh color (intercept p1 z1 p3 z3 l) (intercept p2 z2 p3 z3 l)
           )
           else (* z2 < l < z1 *)
             line fh color (intercept p1 z1 p2 z2 l)
               (if level_eq l z3 then p3
                else if l < z3 then intercept p2 z2 p3 z3 l
                else (* l > z3 *)   intercept p1 z1 p3 z3 l)
         )
         else (* l > z1 *) (
           (* Symmetric of [l < z1] with all inequalities reversed *)
           if level_eq l z2 then
             if level_eq l z3 then
               (if not(Edge.on bd i2 i3) then line fh color p2 p3)
             else if l < z3 then (* z1 < l = z2 < z3 *)
               line fh color p2 (intercept p1 z1 p3 z3 l)
             else (* isolated point, inside the domain *)
               (if marker.{i2} = 0 then point fh i2 p2)
           else if l > z2 then (
             if level_eq l z3 then
               (if marker.{i3} = 0 then point fh i3 p3)
             else if l < z3 then
               line fh color (intercept p1 z1 p3 z3 l) (intercept p2 z2 p3 z3 l)
           )
           else (* z1 < l < z2 *)
             line fh color (intercept p1 z1 p2 z2 l)
               (if level_eq l z3 then p3
                else if l > z3 then intercept p2 z2 p3 z3 l
                else (* l < z3 *)   intercept p1 z1 p3 z3 l)
         )
      ) levels
  done;
  (* Write trailer *)
  latex_end fh;
  close_out fh


(* Local Variables: *)
(* compile-command: "make -k" *)
(* End: *)
