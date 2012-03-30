open Bigarray
open Graphics
module M = Mesh_triangle

let pslg =
  object
    inherit [_] M.pslg fortran_layout
    method point = Array2.of_array float64 fortran_layout
                                   [| [| 0.; 1.; 1.; 0. |];
                                      [| 0.; 0.; 1.; 1. |] |]
    method segment = Array2.of_array int fortran_layout
                                     [| [| 1; 2; 3; 4 |];
                                        [| 2; 3; 4; 1 |] |]
  end


let square, _ = M.triangulate pslg ~max_area:0.3 ~debug:false

let square', _ = M.refine square ~max_area:0.1 ~debug:false

let () =
  open_graph " 880x440-30+10";
  moveto 10 10;
  (try set_font "-*-helvetica-bold-r-*--25-*-*-*-p-*-*-*" with _ -> ());
  let point_idx i = set_color 0xFF0000;
                    draw_string(string_of_int i) in
  Mesh_display.draw square ~width:400 ~height:400 ~point_idx;
  moveto 460 10;
  Mesh_display.draw square' ~width:400 ~height:400 ~point_idx;

  ignore(wait_next_event [Button_down; Key_pressed]);
  close_graph()
