open Printf
open Bigarray

let square_pslg r1 r2 =
  (* Square ]-r1, r1[² inside ]-r2, r2[² *)
  object
    inherit [_] Mesh_triangle.pslg fortran_layout
    method point =
      Array2.of_array float64 fortran_layout
                      [| [| -. r1;   r1; r1; -. r1;  -. r2;   r2; r2; -. r2 |];
                         [| -. r1; -. r1; r1;   r1;  -. r2; -. r2; r2;   r2 |] |]

    method segment =
      Array2.of_array int fortran_layout
                      [| [| 1; 2; 3; 4;  5; 6; 7; 8 |];
                         [| 2; 3; 4; 1;  6; 7; 8; 5 |] |]
    method region =
      Array2.of_array float64 fortran_layout
                      [| [| 0.;            r2 |];
                         [| 0.;            r2 |];
                         [| 1.;            2. |];
                         [| 0.01 *. r1**2.; 0.01 *. r2**2. |] |]
    method point_attribute =
      Array2.of_array float64 fortran_layout
                      [| [| -1.; -2.; -3.; -4.; -5.; -6.; -7.; -8. |] |]
  end

let square r1 r2 =
  fst(Mesh_triangle.triangulate (square_pslg r1 r2)
                                ~debug:false
                                ~region_area:true)

let () =
  let m = square 1. 2. in
  printf "#triangle_attribute = %i\n%!" (Array2.dim1 m#triangle_attribute);
  let triangle_idx t =
    let att = m#triangle_attribute.{1,t} in
    Graphics.set_color (if att = 1. then Graphics.red
                        else if att = 2. then Graphics.blue
                        else Graphics.green);
    Graphics.draw_string (sprintf "%g" att);
    Graphics.set_color Graphics.black in
  Mesh_display.display m ~triangle_idx
