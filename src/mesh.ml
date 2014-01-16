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
    MeshC.band_height_P1 filter (Obj.magic(mesh: _ #t) : c_layout t)
  else
    MeshF.band_height_P1 filter (Obj.magic(mesh: _ #t) : fortran_layout t)

let cuthill_mckee ?(rev=true) ?(perm: 'l int_vec option) (mesh: 'l #t) =
  if is_c_layout mesh then
    let m = MeshC.cuthill_mckee
              ~rev
              (Obj.magic perm : c_layout int_vec option)
              (Obj.magic mesh : c_layout #t) in
    (Obj.magic (m: c_layout t) : 'l t)
  else
    let m = MeshF.cuthill_mckee
              ~rev
              (Obj.magic perm : fortran_layout int_vec option)
              (Obj.magic mesh : fortran_layout #t) in
    (Obj.magic (m: fortran_layout t) : 'l t)

let permute_points ?(inv=false) (perm: 'l int_vec) (mesh: 'l #t) =
  if is_c_layout mesh then
    let m = MeshC.permute_points inv (Obj.magic perm : c_layout int_vec)
                                 (Obj.magic mesh :  c_layout #t) in
    (Obj.magic (m: c_layout t) : 'l t)
  else
    let m = MeshF.permute_points inv (Obj.magic perm : fortran_layout int_vec)
                                 (Obj.magic mesh : fortran_layout #t) in
    (Obj.magic (m: fortran_layout t) : 'l t)


module LaTeX =
struct
  type color = int

  let save ?edge (mesh: _ #t) filename =
    if is_c_layout(mesh :> _ pslg)
    then MeshC.latex ?edge (Obj.magic mesh) filename
    else MeshF.latex ?edge (Obj.magic mesh) filename

  let level_curves ?boundary (mesh: 'a #t) (z: 'a vec)
      ?level_eq levels filename =
    if is_c_layout(mesh :> _ pslg) then
      MeshC.level_curves ?boundary (Obj.magic mesh) (Obj.magic z)
                         ?level_eq levels filename
    else
      MeshF.level_curves ?boundary (Obj.magic mesh) (Obj.magic z)
                         ?level_eq levels filename

  let super_level ?boundary (mesh: 'a #t) (z: 'a vec) level color filename =
    if is_c_layout mesh then
      MeshC.super_level ?boundary (Obj.magic mesh) (Obj.magic z)
                        level color filename
    else
      MeshF.super_level ?boundary (Obj.magic mesh) (Obj.magic z)
                        level color filename

  let sub_level ?boundary (mesh: 'a #t) (z: 'a vec) level color filename =
    if is_c_layout mesh then
      MeshC.sub_level ?boundary (Obj.magic mesh) (Obj.magic z)
                      level color filename
    else
      MeshF.sub_level ?boundary (Obj.magic mesh) (Obj.magic z)
                      level color filename
end

let scilab (mesh: 'a #t) (z: 'a vec) filename =
  if is_c_layout mesh
  then MeshC.scilab (Obj.magic mesh) (Obj.magic z) filename
  else MeshF.scilab (Obj.magic mesh) (Obj.magic z) filename

let matlab (mesh: 'a #t) ?edgecolor ?linestyle ?facealpha (z: 'a vec) filename =
  if is_c_layout mesh
  then MeshC.matlab (Obj.magic mesh) ?edgecolor ?linestyle ?facealpha
                    (Obj.magic z) filename
  else MeshF.matlab (Obj.magic mesh) ?edgecolor ?linestyle ?facealpha
                    (Obj.magic z) filename

let mathematica (mesh: 'a #t) (z: 'a vec) filename =
  if is_c_layout mesh
  then MeshC.mathematica (Obj.magic mesh: _ t) (Obj.magic z: _ vec) filename
  else MeshF.mathematica (Obj.magic mesh: _ t) (Obj.magic z: _ vec) filename


(* Local Variables: *)
(* compile-command: "make -k mesh.cmo" *)
(* End: *)
