(* File: mesh_triangle_common.ml

   Copyright (C) 2014

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)


open Bigarray

class ['l] pslg (layout: 'l layout) =
object
  inherit [_] Mesh.pslg layout
  method point_attribute = Array2.create float64 layout 0 0
end

class ['l] t (mesh: 'l #Mesh.t) =
  let layout = Mesh.layout mesh in
  object
    method point = mesh#point
    method point_marker = mesh#point_marker
    method segment = mesh#segment
    method segment_marker = mesh#segment_marker
    method hole = mesh#hole
    method region = mesh#region
    method triangle = mesh#triangle
    method neighbor = mesh#neighbor
    method edge = mesh#edge
    method edge_marker = mesh#edge_marker

    method point_attribute = Array2.create float64 layout 0 0
    method triangle_attribute = Array2.create float64 layout 0 0
  end

let mesh_to_c (m: _ #t) = (Obj.magic m : c_layout t)
let mesh_to_fortran (m: _ #t) = (Obj.magic m : fortran_layout t)

let mesh_transform (mesh: 'l #t) f_c f_fortran =
  if Mesh_common.is_c_layout mesh then
    let mesh' : c_layout t = f_c (mesh_to_c mesh) in
    (Obj.magic mesh' : 'l t)
  else
    let mesh' : fortran_layout t = f_fortran (mesh_to_fortran mesh) in
    (Obj.magic mesh' : 'l t)


class ['a] mesh_of_pslg (pslg: 'a pslg) =
  let layout = Array2.layout pslg#point in
  let empty_int_mat : 'a Mesh.int_mat = Array2.create int layout 2 0 in
object
  method point = pslg#point
  method point_marker = pslg#point_marker
  method segment = pslg#segment
  method segment_marker = pslg#segment_marker
  method hole = pslg#hole
  method region = pslg#region

  method triangle = empty_int_mat
  method neighbor = empty_int_mat
  method edge = empty_int_mat
  method edge_marker = Array1.create int layout 0

  method point_attribute = pslg#point_attribute
  method triangle_attribute = Array2.create float64 layout 2 0
end

class type ['l] voronoi =
object
  inherit ['l] Mesh.voronoi
  method point_attribute : 'l Mesh.mat
end

type triunsuitable =
  float -> float -> float -> float -> float -> float -> float -> bool

let register_triunsuitable (f: triunsuitable) =
  Callback.register "triunsuitable_callback" f

exception Invalid_argument of string

let invalid_arg m = raise(Invalid_argument m)

let is_finite x = neg_infinity < x && x < infinity (* => is not NaN *)

