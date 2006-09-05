(* Functions for LAYOUT layout.
 ***********************************************************************)

let bounding_box_LAYOUT (mesh : LAYOUT_layout t) =
  let xmin = ref infinity
  and xmax = ref neg_infinity
  and ymin = ref infinity
  and ymax = ref neg_infinity in
  for i = BA_FIRST to BA_LASTCOL(mesh.point) do
    let x = BA_GET(mesh.point,BA_FIRST,i)
    and y = BA_GET(mesh.point,BA_SECOND,i) in
    if x > !xmax then xmax := x;
    if x < !xmin then xmin := x;
    if y > !ymax then ymax := y;
    if y < !ymin then ymin := y;
  done;
  (!xmin, !xmax, !ymin, !ymax)

let latex_LAYOUT (mesh : LAYOUT_layout t) filename =
  let fh = open_out filename in
  let xmin, xmax, ymin, ymax = bounding_box_LAYOUT mesh in
  latex_begin fh (xmax -. xmin) (ymax -. ymin) xmin ymin;
  (* Write lines *)
  fprintf fh "  %% %i edges\n" (BA_NCOL(mesh.triangle));
  for e = BA_FIRST to BA_LASTCOL(mesh.edge) do
    let i1 = BA_GET(mesh.edge,BA_FIRST,e)
    and i2 = BA_GET(mesh.edge,BA_SECOND,e) in
    let x1 = BA_GET(mesh.point,BA_FIRST,i1)
    and y1 = BA_GET(mesh.point,BA_SECOND,i1)
    and x2 = BA_GET(mesh.point,BA_FIRST,i2)
    and y2 = BA_GET(mesh.point,BA_SECOND,i2) in
    line fh "black" (x1, y1) (x2, y2)
  done;
  (* Write points *)
  fprintf fh "  %% %i points\n" (BA_NCOL(mesh.point));
  for i = BA_FIRST to BA_LASTCOL(mesh.point) do
    point fh i BA_GET(mesh.point,BA_FIRST,i) BA_GET(mesh.point,BA_SECOND,i);
  done;
  latex_end fh;
  close_out fh


let scilab_LAYOUT (mesh : LAYOUT_layout t) (z: LAYOUT_layout vec) fname =
  let fname = Filename.chop_extension fname in
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
    for point = BA_FIRST to BA_THIRD do
      for t = BA_FIRST to BA_LASTCOL(mesh.triangle) do
        fprintf fh "%.13g " (coord BA_GET(mesh.triangle,point,t))
      done;
      fprintf fh "\n";
    done;
    close_out fh in
  let pt = mesh.point in
  save_mat xf (fun i -> BA_GET(pt,BA_FIRST,i));
  save_mat yf (fun i -> BA_GET(pt,BA_SECOND,i));
  save_mat zf (fun i -> z.{i})


let level_curves_LAYOUT ?(boundary=(fun _ -> Some "black"))
    (mesh : LAYOUT_layout t) (z: LAYOUT_layout vec) levels fname =
  let fh = open_out fname in
  let xmin, xmax, ymin, ymax = bounding_box_LAYOUT mesh in
  latex_begin fh (xmax -. xmin) (ymax -. ymin) xmin ymin;
  (* Draw the boundaries *)
  let edge = mesh.edge in
  let marker = mesh.edge_marker in
  for e = BA_FIRST to BA_LASTCOL(edge) do
    let m = marker.{e} in
    if m <> 0 then begin
      match boundary m with
      | None -> ()
      | Some color ->
          let i1 = BA_GET(mesh.edge,BA_FIRST,e)
          and i2 = BA_GET(mesh.edge,BA_SECOND,e) in
          let x1 = BA_GET(mesh.point,BA_FIRST,i1)
          and y1 = BA_GET(mesh.point,BA_SECOND,i1)
          and x2 = BA_GET(mesh.point,BA_FIRST,i2)
          and y2 = BA_GET(mesh.point,BA_SECOND,i2) in
          line fh color (x1, y1) (x2, y2)
    end
  done;
  let tr = mesh.triangle in
  let pt = mesh.point in
  for t = BA_FIRST to BA_LASTCOL(tr) do
    let i1 = BA_GET(tr,BA_FIRST,t)
    and i2 = BA_GET(tr,BA_SECOND,t)
    and i3 = BA_GET(tr,BA_THIRD,t) in
    let x1 = BA_GET(pt,BA_FIRST,i1)
    and y1 = BA_GET(pt,BA_SECOND,i1)
    and z1 = z.{i1} in
    let x2 = BA_GET(pt,BA_FIRST,i2)
    and y2 = BA_GET(pt,BA_SECOND,i2)
    and z2 = z.{i2} in
    let x3 = BA_GET(pt,BA_FIRST,i3)
    and y3 = BA_GET(pt,BA_SECOND,i3)
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


