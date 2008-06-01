(* Functions for LAYOUT layout.
 ***********************************************************************)

(** Return the smaller box (xmin, xmax, ymin, ymax) containing the [mesh]. *)
let bounding_box (mesh: mesh) =
  let xmin = ref infinity
  and xmax = ref neg_infinity
  and ymin = ref infinity
  and ymax = ref neg_infinity in
  for i = FST to LASTCOL(mesh.point) do
    let x = GET(mesh.point, FST,i)
    and y = GET(mesh.point, SND,i) in
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
  fprintf fh "  %% %i edges\n" (NCOLS(mesh.triangle));
  for e = FST to LASTCOL(mesh.edge) do
    let i1 = GET(mesh.edge, FST,e)
    and i2 = GET(mesh.edge, SND,e) in
    let x1 = GET(mesh.point, FST,i1)
    and y1 = GET(mesh.point, SND,i1)
    and x2 = GET(mesh.point, FST,i2)
    and y2 = GET(mesh.point, SND,i2) in
    line fh "black" (x1, y1) (x2, y2)
  done;
  (* Write points *)
  fprintf fh "  %% %i points\n" (NCOLS(mesh.point));
  for i = FST to LASTCOL(mesh.point) do
    point fh i (GET(mesh.point, FST,i)) (GET(mesh.point, SND,i));
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
    for point = FST to THIRD do
      for t = FST to LASTCOL(mesh.triangle) do
        fprintf fh "%.13g " (coord (GET(mesh.triangle, point,t)))
      done;
      fprintf fh "\n";
    done;
    close_out fh in
  let pt = mesh.point in
  save_mat xf (fun i -> GET(pt, FST,i));
  save_mat yf (fun i -> GET(pt, SND,i));
  save_mat zf (fun i -> z.{i})

let matlab (mesh: mesh) (z: vec) fname =
  let fname = try Filename.chop_extension fname with _ -> fname in
  let pt = mesh.point in
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
  let tr = mesh.triangle in
  for t = FST to LASTCOL(mesh.triangle) do
    fprintf fh "%i %i %i; " (GET(tr, FST,t)) (GET(tr, SND,t)) (GET(tr, THIRD,t))
    done;
  fprintf fh "];\ntrisurf(mesh_triangles, mesh_x, mesh_y, mesh_z);\n";
  close_out fh
;;


let level_curves ?(boundary=(fun _ -> Some "black")) (mesh: mesh) (z: vec)
    levels fname =
  let fh = open_out fname in
  let xmin, xmax, ymin, ymax = bounding_box mesh in
  latex_begin fh (xmax -. xmin) (ymax -. ymin) xmin ymin;
  (* Draw the boundaries *)
  let edge = mesh.edge in
  let marker = mesh.edge_marker in
  for e = FST to LASTCOL(edge) do
    let m = marker.{e} in
    if m <> 0 (* not an interior point *) then begin
      match boundary m with
      | None -> ()
      | Some color ->
          let i1 = GET(mesh.edge, FST,e)
          and i2 = GET(mesh.edge, SND,e) in
          let x1 = GET(mesh.point, FST,i1)
          and y1 = GET(mesh.point, SND,i1)
          and x2 = GET(mesh.point, FST,i2)
          and y2 = GET(mesh.point, SND,i2) in
          line fh color (x1, y1) (x2, y2)
    end
  done;
  let tr = mesh.triangle in
  let pt = mesh.point in
  for t = FST to LASTCOL(tr) do
    let i1 = GET(tr, FST,t)
    and i2 = GET(tr, SND,t)
    and i3 = GET(tr, THIRD,t) in
    let x1 = GET(pt, FST,i1)
    and y1 = GET(pt, SND,i1)
    and z1 = z.{i1} in
    let x2 = GET(pt, FST,i2)
    and y2 = GET(pt, SND,i2)
    and z2 = z.{i2} in
    let x3 = GET(pt, FST,i3)
    and y3 = GET(pt, SND,i3)
    and z3 = z.{i3} in
    List.iter
      (fun l ->
         (* Draw the level curve [l] on the triangle [t]. *)
         if l < z1 then
           if l < z2 then
             if l > z3 then
               line fh "black" (intercept x1 y1 z1 x3 y3 z3 l)
                 (intercept x2 y2 z2 x3 y3 z3 l)
             else if l = z3 then
               point fh i3 x3 y3
             else ()
           else if l = z2 then
             if l >= z3 then (* z3 <= l = z2 < z1 *)
               line fh "black" (x2,y2) (intercept x1 y1 z1 x3 y3 z3 l)
             else
               point fh i2 x2 y2
           else (* l > z2 *)
             line fh "black" (intercept x1 y1 z1 x2 y2 z2 l)
               (if l < z3 then      intercept x2 y2 z2 x3 y3 z3 l
                else if l > z3 then intercept x1 y1 z1 x3 y3 z3 l
                else (* l = z3 *)   (x3,y3))

         else if l > z1 then
           (* Symmetric of [l < z1] with all inequalities reversed *)
           if l > z2 then
             if l < z3 then
               line fh "black" (intercept x1 y1 z1 x3 y3 z3 l)
                 (intercept x2 y2 z2 x3 y3 z3 l)
             else if l = z3 then
               point fh i3 x3 y3
             else ()
           else if l = z2 then
             if l <= z3 then (* z3 >= l = z2 > z1 *)
               line fh "black" (x2,y2) (intercept x1 y1 z1 x3 y3 z3 l)
             else
               point fh i2 x2 y2
           else (* l < z2 *)
             line fh "black" (intercept x1 y1 z1 x2 y2 z2 l)
               (if l > z3 then      intercept x2 y2 z2 x3 y3 z3 l
                else if l < z3 then intercept x1 y1 z1 x3 y3 z3 l
                else (* l = z3 *)   (x3,y3))

         else (* l = z1 *)
           if (z2 <= l && l < z3) || (z3 <= l && l < z2) then
             line fh "black" (x1,y1) (intercept x2 y2 z2 x3 y3 z3 l)
           else if l = z2 && l = z3 then
             triangle fh "black" x1 y1 x2 y2 x3 y3 (* Full triangle ! *)
           else ()
      ) levels
  done;
  (* Write trailer *)
  latex_end fh;
  close_out fh
