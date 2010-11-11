(*pp camlp4o pa_macro.cmo *)
(* File: triangle.ml

   Copyright (C) 2009

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

external init : unit -> unit = "ocaml_triangle_init"
let () = init()

class type ['l] pslg =
object
  inherit ['l] Mesh.pslg
  method point_attribute : 'l Mesh.mat
end

class type ['l] t =
object
  inherit ['l] Mesh.t
  method point_attribute : 'l Mesh.mat
  method triangle_attribute : 'l Mesh.mat
end

class type ['l] voronoi =
object
  inherit ['l] Mesh.voronoi
  method point_attribute : 'l Mesh.mat
end

type triunsuitable =
  float -> float -> float -> float -> float -> float -> float -> bool

let triunsuitable : triunsuitable -> unit =
  Callback.register "triunsuitable_callback"

exception Invalid_argument of string

let invalid_arg m = raise(Invalid_argument m)

module F =
struct
  type layout = fortran_layout
  type mat = layout Mesh.mat
  type vec = layout Mesh.vec
  type int_mat = layout Mesh.int_mat
  type int_vec = layout Mesh.int_vec

  let layout = fortran_layout
  let default_switches = ""

  external triangle :
    string ->                        (* options *)
    layout t ->
    vec                              (* trianglearea *)
    -> mat * mat * int_vec * int_mat * mat * int_mat * int_mat * int_vec
      * (* edge *) int_mat * int_vec
      * (* voronoi *) mat * mat * int_mat * mat
      = "triangulate_fortran_layout"

  DEFINE NCOLS(a) = Array2.dim2 a;;
  DEFINE NROWS(a) = Array2.dim1 a;;
  DEFINE COLS = "dim2";;
  DEFINE ROWS = "dim1";;
  INCLUDE "mesh_triangleFC.ml";;
end

module C =
struct
  type layout = c_layout
  type mat = layout Mesh.mat
  type vec = layout Mesh.vec
  type int_mat = layout Mesh.int_mat
  type int_vec = layout Mesh.int_vec

  let layout = c_layout
  let default_switches = "z"

  external triangle :
    string ->                        (* options *)
    layout t ->
    vec                              (* trianglearea *)
    -> mat * mat * int_vec * int_mat * mat * int_mat * int_mat * int_vec
      * (* edge *) int_mat * int_vec
      * (* voronoi *) mat * mat * int_mat * mat
      = "triangulate_c_layout"

  DEFINE NCOLS(a) = Array2.dim1 a;;
  DEFINE NROWS(a) = Array2.dim2 a;;
  DEFINE COLS = "dim1";;
  DEFINE ROWS = "dim2";;
  INCLUDE "mesh_triangleFC.ml";;
end

let triangulate ?refine ?triangle_attribute
    ?min_angle ?max_area ?convex_hull ?max_steiner ?voronoi ?edge
    ?subparam ?triangle_area mesh =
  let layout = Array2.layout mesh#point in
  if (Obj.magic layout) = fortran_layout then
    let triangle_area = match triangle_area with
      | None -> None
      | Some v -> Some((Obj.magic(v: 'a Mesh.vec)) : F.layout Mesh.vec) in
    let res =
      F.triangulate ?refine ?triangle_attribute
        ?min_angle ?max_area ?convex_hull ?max_steiner ?voronoi ?edge
        ?subparam ?triangle_area false false
        ((Obj.magic(mesh: 'a t)) : F.layout t) in
    (Obj.magic(res:F.layout t * F.layout voronoi) : 'a t * 'a voronoi)
  else
    let triangle_area = match triangle_area with
      | None -> None
      | Some v -> Some((Obj.magic(v: 'a Mesh.vec)) : C.layout Mesh.vec) in
    let res =
      C.triangulate ?refine ?triangle_attribute
        ?min_angle ?max_area ?convex_hull ?max_steiner ?voronoi ?edge
        ?subparam ?triangle_area false false
        ((Obj.magic(mesh: 'a t)) : C.layout t) in
    (Obj.magic(res:C.layout t * C.layout voronoi) : 'a t * 'a voronoi)




(* Loading various formats *)



(* Save to triangle format *)
let save mesh filename =
  (* .node file *)
  let fh = open_out (filename ^ ".node") in
  close_out fh
  (* .ele file *)
  (* .poly file *)
  (* .edge file *)
  (* .neigh file *)
