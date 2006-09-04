(* Displaying meshes -- gathering the FORTRAN and C layouts together
 ***********************************************************************)

let display ?width ?height ?xmin ?xmax ?ymin ?ymax
    ?xbd ?ybd ?voronoi ?segments (mesh: 'a t) =
  if Mesh.is_c_layout mesh then
    Obj.magic (display_c ?width ?height ?xmin ?xmax ?ymin ?ymax
                 ?xbd ?ybd ?voronoi ?segments (Obj.magic mesh))
  else
    Obj.magic (display_fortran ?width ?height ?xmin ?xmax ?ymin ?ymax
                 ?xbd ?ybd ?voronoi ?segments (Obj.magic mesh))
