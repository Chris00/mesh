(* File: triangle.mli

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

(** Interface for the Triangle 2D mesh generator.
 * http://www.cs.cmu.edu/~quake/triangle.html
 *)

type 'l voronoi = {
  vor_point : 'l Mesh.mat;
  vor_point_attribute : 'l Mesh.mat;
  vor_edge : 'l Mesh.int_mat;
  vor_normal : 'l Mesh.mat;
}

val triangulate :
  ?point_attribute:'a Mesh.mat ->
  ?refine:bool ->
  ?triangle_attribute:'a Mesh.mat ->
  ?min_angle:float ->
  ?max_area:float ->
  ?convex_hull:bool ->
  ?max_steiner:int ->
  ?voronoi:bool ->
  ?edge:bool ->
  ?subparam:bool ->
  ?triangle_area:'a Mesh.vec ->
  'a Mesh.t -> 'a Mesh.t * 'a voronoi
  (** [triangulate pslg] returns a triangulation of the domain
      described by [pslg].   *)
