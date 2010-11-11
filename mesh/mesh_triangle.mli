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

class type ['l] pslg =
object
  inherit ['l] Mesh.pslg

  method point_attribute : 'l Mesh.mat
  (** A matrix of size [a * n] ([fortran_layout]) where [a] is the
      number of attributes per point and [n] is the number of points. *)
end

class type ['l] refinable =
object
  inherit ['l] Mesh.t

  method triangle_area : 'l Mesh.vec
  (** A vector of triangle area constraints. *)
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
