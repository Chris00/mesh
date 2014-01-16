open Bigarray

(* Useful conversion functions to avoid Obj.magic at many places while
   being more secure. *)
external vec_to_c : ('a, 'b, 'c) Array1.t -> ('a, 'b, c_layout) Array1.t
  = "%identity"
external vec_to_fortran : ('a, 'b, 'c) Array1.t -> ('a, 'b, fortran_layout) Array1.t
  = "%identity"
external mat_to_c : ('a, 'b, 'c) Array2.t -> ('a, 'b, c_layout) Array2.t
  = "%identity"
external mat_to_fortran : ('a, 'b, 'c) Array2.t -> ('a, 'b, fortran_layout) Array2.t
  = "%identity"

external vec_opt_to_c :
  ('a, 'b, 'c) Array1.t option -> ('a, 'b, c_layout) Array1.t option
  = "%identity"
external vec_opt_to_fortran :
  ('a, 'b, 'c) Array1.t option -> ('a, 'b, fortran_layout) Array1.t option
  = "%identity"

let is_c_layout (l: _ layout) =
  l = (Obj.magic c_layout : _ layout)
