(*
#load "bigarray.cma";;
#load "graphics.cma";;
#load "mesh.cma";;
*)

open Bigarray
open Printf

module M = Easymesh

let pi = 4. *. atan 1.

let () =
  let m = M.triangulate ~max_area:0.1 {
    (Mesh.empty fortran_layout) with
      (* Fortran layout: point coordinates in columns *)
      Mesh.point = Array2.of_array float64 fortran_layout
      [| [| 0.0;  1.0;  0.5 |];
         [| 0.0;  0.0;  1.0 |] |];
      (* Fortran layout: segment enpoints in columns *)
      Mesh.segment = Array2.of_array int fortran_layout
      [| [| 1;  2;  3 |];
         [| 2;  3;  1 |] |];
  } in
  Mesh.latex m "/tmp/testmesh.tex"; (* \setlength{\unitlength}{40mm}
                                       to see something. *)
(*   Mesh_display.display m; *)

  (* Save the graph of [f] on [m] so it can be displayed by Scilab.  *)
  let f x y = sin(pi *. (2. *. x -. y)) *. sin(pi *. y) in
  let fname = "/tmp/triangle" in
  let sci = fname ^ ".sci"
  and xf = fname ^ "_x.dat"
  and yf = fname ^ "_y.dat"
  and zf = fname ^ "_z.dat" in
  let fh = open_out sci in
  fprintf fh "// Run with exec('%s')
xf = fscanfMat('%s');
yf = fscanfMat('%s');
zf = fscanfMat('%s');
plot3d(xf', yf', zf')\n" sci xf yf zf;
  close_out fh;

  let tr = m.Mesh.triangle
  and pt = m.Mesh.point in
  let save_mat fname coord =
    let fh = open_out fname in
    for t = 1 to Array2.dim2 tr do
      let i1 = tr.{1,t}
      and i2 = tr.{2,t}
      and i3 = tr.{3,t} in
      fprintf fh "%.13g %.13g %.13g\n" (coord i1) (coord i2) (coord i3)
    done;
    close_out fh in

  save_mat xf (fun i -> pt.{1,i});
  save_mat yf (fun i -> pt.{2,i});
  save_mat zf (fun i -> f pt.{1,i} pt.{2,i});
  printf "Run Scilab script with: exec('%s')\n" sci
