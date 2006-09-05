
let is_c_layout mesh =
  Array2.layout mesh.point = (Obj.magic c_layout : 'a Bigarray.layout)

let latex mesh filename =
  if is_c_layout mesh then latex_c (Obj.magic mesh) filename
  else latex_fortran (Obj.magic mesh) filename

let scilab (mesh: 'a t) (z: 'a vec) filename =
  if is_c_layout mesh then scilab_c (Obj.magic mesh) (Obj.magic z) filename
  else scilab_fortran (Obj.magic mesh) (Obj.magic z) filename

let level_curves ?boundary (mesh: 'a t) (z: 'a vec) levels filename =
  if is_c_layout mesh then
    level_curves_c ?boundary (Obj.magic mesh) (Obj.magic z) levels filename
  else
    level_curves_fortran ?boundary (Obj.magic mesh) (Obj.magic z)
      levels filename
