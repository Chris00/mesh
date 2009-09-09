open Bigarray
open Lacaml.Impl.D
open Printf
module L = Laplacian

let pi = 4. *. atan 1.

let mesh_square =
  let pt = Array2.of_array float64 fortran_layout
    [| [| 0.; 1.; 1.; 0. |];   (* x *)
       [| 0.; 0.; 1.; 1. |] |] (* y *) in
  let edges = Array2.of_array int fortran_layout
    [| [| 1; 2; 3; 4 |];   (* start *)
       [| 2; 3; 4; 1 |] |] (* end *) in
  let plsg = object
    inherit [_] Mesh.pslg fortran_layout
    method point = pt
    method segment = edges
  end in
  Easymesh.triangulate plsg ~max_area:0.03

let square = L.make mesh_square ~bc:(fun _ _ _ -> Some 0.)


let () =
  printf "The mesh has %i nodes.\n%!" (L.nnodes square);
  printf "Press 'q' on its window to quit the mesh display.%!";
  Mesh_display.display (L.mesh square);
  printf "\n";

  let area = ref 0. in
  for i = 1 to Array2.dim2 (L.mesh square)#triangle do
    area := !area +. L.area square i;
  done;
  printf "total area = %g\n" !area;

  let sol x y = sin(pi *. x) *. sin(pi *. y) in
  let u = L.vec_of_fun square sol in
  printf "norm^2 = %g (exact %g)\n" (L.norm2 square u) (pi**2. /. 2.);
  let integ = L.integrate square (fun _ _ u -> u) u in
  printf "integ u = %g (exact %g)\n" integ (4. /. pi**2.);

  let rhs x y = 2. *. pi**2. *. sol x y in
  let u = L.nonlin square (fun x y _ -> rhs x y) u in
  printf "integ rhs = %g (exact 8)\n" (Vec.sum u);
  L.solve square u;
  printf "norm^2 sol = %g (exact %g)\n" (L.norm2 square u) (pi**2. /. 2.);

  let fname = "/tmp/square" in
  Mesh.matlab (L.mesh square) u fname;
  printf "Display the graph in Matlab with: run('%s');\n" fname
