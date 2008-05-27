(*
#load "bigarray.cma";;
#load "graphics.cma";;
#load "mesh.cma";;
*)

open Bigarray
open Printf

module M = Easymesh

let pi = 4. *. atan 1.

(* Round [x] to the nearest integer *)
let round x = floor(x +. 0.5)

let mesh = M.triangulate ~max_area:0.05
  { (Mesh.empty fortran_layout) with
      (* Fortran layout: point coordinates in columns *)
      Mesh.point = Array2.of_array float64 fortran_layout
      [| [| 0.0;  1.0;  0.5 |];
         [| 0.0;  0.0;  1.0 |] |];
      (* Fortran layout: segment enpoints in columns *)
      Mesh.segment = Array2.of_array int fortran_layout
      [| [| 1;  2;  3 |];
         [| 2;  3;  1 |] |];
  }

let () =
  Mesh_display.display mesh;
  Mesh.latex mesh "/tmp/testmesh.tex";

  (* Save the graph of [f] on [m] so it can be displayed by Scilab.  *)
  let f x y = sin(pi *. (2. *. x -. y)) *. sin(pi *. y) in
  let pt = mesh.Mesh.point in
  let z = Array1.create float64 fortran_layout (Array2.dim2 pt) in
  for i = 1 to Array2.dim2 pt do z.{i} <- f pt.{1,i} pt.{2,i} done;
  let sci = "/tmp/triangle.sci" in
  Mesh.scilab mesh z sci;
  printf "Run Scilab script with: exec('%s')\n" sci;

  (* Round to 4 decimal places *)
  for i = 1 to Array2.dim2 pt do z.{i} <- round(1e4 *. z.{i}) *. 1e-4 done;

  List.iter (fun i -> printf "z.{%i} = %g\n" i z.{i}) [1;2;3; 196];
  Mesh.level_curves ~boundary:(fun _ -> Some "red")
    mesh z [-0.5; -0.2; 0.; 0.1; 0.5; 0.8; 0.95] "/tmp/levels.tex"
