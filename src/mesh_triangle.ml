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
open Mesh_utils

external init : unit -> unit = "ocaml_triangle_init"
let () = init()

include Mesh_triangle_common

let triangle ?delaunay ?min_angle ?max_area ?region_area ?max_steiner
             ?voronoi ?edge ?neighbor ?subparam ?triangle_area
             ?check_finite ?debug ?triunsuitable
             ~pslg ~refine mesh =
  let layout = Array2.layout mesh#point in
  if is_c_layout layout then
    let triangle_area = match triangle_area with
      | None -> None
      | Some v -> Some(vec_to_c v) in
    let res =
      Mesh_triangleC.triangulate
        ?delaunay
        ?min_angle ?max_area ?region_area ?max_steiner ?voronoi ?neighbor ?edge
        ?subparam ?triangle_area ?triunsuitable ?check_finite ?debug
        ~pslg ~refine (mesh_to_c mesh) in
    (Obj.magic(res:c_layout t * c_layout voronoi) : 'a t * 'a voronoi)
  else
    let triangle_area = match triangle_area with
      | None -> None
      | Some v -> Some(vec_to_fortran v) in
    let res =
      Mesh_triangleF.triangulate
        ?delaunay
        ?min_angle ?max_area ?region_area ?max_steiner ?voronoi ?neighbor ?edge
        ?subparam ?triangle_area ?triunsuitable ?check_finite ?debug
        ~pslg ~refine (mesh_to_fortran mesh) in
    (Obj.magic(res:fortran_layout t * fortran_layout voronoi) : 'a t * 'a voronoi)


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



let permute_points (old_mesh: 'l #t) ?inv perm : 'l t =
  let mesh = Mesh.permute_points old_mesh ?inv perm in
  let attr =
    if Mesh.is_c_layout mesh then
      Obj.magic(Mesh_triangleC.do_permute_point_attribute
                  (mat_to_c old_mesh#point_attribute)
                  (vec_to_c perm))
    else
      Obj.magic(Mesh_triangleF.do_permute_point_attribute
                  (mat_to_fortran old_mesh#point_attribute)
                  (vec_to_fortran perm)) in
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

    method point_attribute = attr
    method triangle_attribute = old_mesh#triangle_attribute
  end


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
