(* File: mesh.ml

   Copyright (C) 2006

     Christophe Troestler
     email: Christophe.Troestler@umh.ac.be
     WWW: http://math.umh.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)


open Bigarray
open Printf

include Mesh_common

let band_height_P1 ?filter mesh =
  if is_c_layout mesh then
    Mesh_C.band_height_P1 filter (Obj.magic(mesh: _ #t) : c_layout t)
  else
    Mesh_F.band_height_P1 filter (Obj.magic(mesh: _ #t) : fortran_layout t)

let cuthill_mckee ?(rev=true) ?(perm: 'l int_vec option) (mesh: 'l #t) =
  if is_c_layout mesh then
    let m = Mesh_C.cuthill_mckee
              ~rev
              (Obj.magic perm : c_layout int_vec option)
              (Obj.magic mesh : c_layout #t) in
    (Obj.magic (m: c_layout t) : 'l t)
  else
    let m = Mesh_F.cuthill_mckee
              ~rev
              (Obj.magic perm : fortran_layout int_vec option)
              (Obj.magic mesh : fortran_layout #t) in
    (Obj.magic (m: fortran_layout t) : 'l t)

let permute_points ?(inv=false) (perm: 'l int_vec) (mesh: 'l #t) =
  if is_c_layout mesh then
    let m = Mesh_C.permute_points inv (Obj.magic perm : c_layout int_vec)
                                 (Obj.magic mesh :  c_layout #t) in
    (Obj.magic (m: c_layout t) : 'l t)
  else
    let m = Mesh_F.permute_points inv (Obj.magic perm : fortran_layout int_vec)
                                 (Obj.magic mesh : fortran_layout #t) in
    (Obj.magic (m: fortran_layout t) : 'l t)


module LaTeX =
struct
  type color = int

  let save ?edge (mesh: _ #t) filename =
    if is_c_layout(mesh :> _ pslg)
    then Mesh_C.latex ?edge (Obj.magic mesh) filename
    else Mesh_F.latex ?edge (Obj.magic mesh) filename

  let level_curves ?boundary (mesh: 'a #t) (z: 'a vec)
      ?level_eq levels filename =
    if is_c_layout(mesh :> _ pslg) then
      Mesh_C.level_curves ?boundary (Obj.magic mesh) (Obj.magic z)
                         ?level_eq levels filename
    else
      Mesh_F.level_curves ?boundary (Obj.magic mesh) (Obj.magic z)
                         ?level_eq levels filename

  let super_level ?boundary (mesh: 'a #t) (z: 'a vec) level color filename =
    if is_c_layout mesh then
      Mesh_C.super_level ?boundary (Obj.magic mesh) (Obj.magic z)
                        level color filename
    else
      Mesh_F.super_level ?boundary (Obj.magic mesh) (Obj.magic z)
                        level color filename

  let sub_level ?boundary (mesh: 'a #t) (z: 'a vec) level color filename =
    if is_c_layout mesh then
      Mesh_C.sub_level ?boundary (Obj.magic mesh) (Obj.magic z)
                      level color filename
    else
      Mesh_F.sub_level ?boundary (Obj.magic mesh) (Obj.magic z)
                      level color filename
end

let scilab (mesh: 'a #t) (z: 'a vec) filename =
  if is_c_layout mesh
  then Mesh_C.scilab (Obj.magic mesh) (Obj.magic z) filename
  else Mesh_F.scilab (Obj.magic mesh) (Obj.magic z) filename

let matlab (mesh: 'a #t) ?edgecolor ?linestyle ?facealpha (z: 'a vec) filename =
  if is_c_layout mesh
  then Mesh_C.matlab (Obj.magic mesh) ?edgecolor ?linestyle ?facealpha
                    (Obj.magic z) filename
  else Mesh_F.matlab (Obj.magic mesh) ?edgecolor ?linestyle ?facealpha
                    (Obj.magic z) filename

let mathematica (mesh: 'a #t) (z: 'a vec) filename =
  if is_c_layout mesh
  then Mesh_C.mathematica (Obj.magic mesh: _ t) (Obj.magic z: _ vec) filename
  else Mesh_F.mathematica (Obj.magic mesh: _ t) (Obj.magic z: _ vec) filename


(* Local Variables: *)
(* compile-command: "make -k mesh.cmo" *)
(* End: *)
