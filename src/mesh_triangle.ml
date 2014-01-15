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

include Mesh_triangle_common

let triangle ?delaunay ?min_angle ?max_area ?region_area ?max_steiner
             ?voronoi ?edge ?neighbor ?subparam ?triangle_area
             ?check_finite ?debug ?triunsuitable
             ~pslg ~refine mesh =
  let layout = Array2.layout mesh#point in
  if (Obj.magic layout) = fortran_layout then
    let triangle_area = match triangle_area with
      | None -> None
      | Some v -> Some((Obj.magic(v: 'a Mesh.vec)) : fortran_layout Mesh.vec) in
    let res =
      Mesh_triangleF.triangulate
        ?delaunay
        ?min_angle ?max_area ?region_area ?max_steiner ?voronoi ?neighbor ?edge
        ?subparam ?triangle_area ?triunsuitable ?check_finite ?debug
        ~pslg ~refine ((Obj.magic(mesh: 'a t)) : fortran_layout t) in
    (Obj.magic(res:fortran_layout t * fortran_layout voronoi) : 'a t * 'a voronoi)
  else
    let triangle_area = match triangle_area with
      | None -> None
      | Some v -> Some((Obj.magic(v: 'a Mesh.vec)) : c_layout Mesh.vec) in
    let res =
      Mesh_triangleC.triangulate
        ?delaunay
        ?min_angle ?max_area ?region_area ?max_steiner ?voronoi ?neighbor ?edge
        ?subparam ?triangle_area ?triunsuitable ?check_finite ?debug
        ~pslg ~refine ((Obj.magic(mesh: 'a t)) : c_layout t) in
    (Obj.magic(res:c_layout t * c_layout voronoi) : 'a t * 'a voronoi)


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
