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
open Printf

external init : unit -> unit = "ocaml_triangle_init"
let () = init()

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
  ;;
  DEFINE FST = 1;;
  DEFINE SND = 2;;
  DEFINE GET(a, i, j) = a.{i,j};; (* FORTRAN view taken as default *)
  DEFINE LINE_COL(f, i, j) = f i j;;
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
  ;;
  DEFINE FST = 0;;
  DEFINE SND = 1;;
  DEFINE GET(a, i, j) = a.{j,i};; (* C matrices are transpose of FORTRAN *)
  DEFINE LINE_COL(f, i, j) = f j i;;
  DEFINE NCOLS(a) = Array2.dim1 a;;
  DEFINE NROWS(a) = Array2.dim2 a;;
  DEFINE COLS = "dim1";;
  DEFINE ROWS = "dim2";;
  INCLUDE "mesh_triangleFC.ml";;
end

let triangle ?delaunay ?min_angle ?max_area ?region_area ?max_steiner
             ?voronoi ?edge ?neighbor ?subparam ?triangle_area
             ?check_finite ?debug ?triunsuitable
             ~pslg ~refine mesh =
  let layout = Array2.layout mesh#point in
  if (Obj.magic layout) = fortran_layout then
    let triangle_area = match triangle_area with
      | None -> None
      | Some v -> Some((Obj.magic(v: 'a Mesh.vec)) : F.layout Mesh.vec) in
    let res =
      F.triangulate ?delaunay
        ?min_angle ?max_area ?region_area ?max_steiner ?voronoi ?neighbor ?edge
        ?subparam ?triangle_area ?triunsuitable ?check_finite ?debug
        ~pslg ~refine ((Obj.magic(mesh: 'a t)) : F.layout t) in
    (Obj.magic(res:F.layout t * F.layout voronoi) : 'a t * 'a voronoi)
  else
    let triangle_area = match triangle_area with
      | None -> None
      | Some v -> Some((Obj.magic(v: 'a Mesh.vec)) : C.layout Mesh.vec) in
    let res =
      C.triangulate ?delaunay
        ?min_angle ?max_area ?region_area ?max_steiner ?voronoi ?neighbor ?edge
        ?subparam ?triangle_area ?triunsuitable ?check_finite ?debug
        ~pslg ~refine ((Obj.magic(mesh: 'a t)) : C.layout t) in
    (Obj.magic(res:C.layout t * C.layout voronoi) : 'a t * 'a voronoi)


let triangulate ?delaunay ?min_angle ?max_area ?region_area ?max_steiner
    ?voronoi ?edge ?neighbor ?subparam ?triunsuitable ?check_finite ?debug
    pslg =
  let mesh = new mesh_of_pslg pslg in
  triangle ?delaunay ?min_angle ?max_area ?region_area ?max_steiner ?voronoi
    ?edge ?neighbor ?subparam ?triunsuitable ?check_finite ?debug
    ~pslg:true ~refine:false mesh

let refine ?delaunay ?min_angle ?max_area ?max_steiner
    ?voronoi ?edge ?neighbor ?subparam ?triangle_area ?triunsuitable
    ?check_finite ?debug mesh =
  triangle ?delaunay ?min_angle ?max_area ?max_steiner ?voronoi
    ?edge ?neighbor ?subparam ?triangle_area ?triunsuitable ?check_finite ?debug
    ~pslg:false ~refine:true mesh


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
