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

class ['l] pslg : 'l Bigarray.layout ->
object
  inherit ['l] Mesh.pslg

  method point_attribute : 'l Mesh.mat
  (** A matrix of size [a * n] ([fortran_layout]) where [a] is the
      number of attributes per point and [n] is the number of points. *)
end

class type ['l] t =
object
  inherit ['l] Mesh.t

  method point_attribute : 'l Mesh.mat
  (** A matrix of size [a * n] ([fortran_layout]) where [a] is the
      number of attributes per point and [n] is the number of points. *)

  method triangle_attribute : 'l Mesh.mat
end

class type ['l] voronoi =
object
  inherit ['l] Mesh.voronoi
  method point_attribute : 'l Mesh.mat
end

exception Invalid_argument of string

type triunsuitable =
  float -> float -> float -> float -> float -> float -> float -> bool
(** Type of functions used to determine whether or not a selected
    triangle is too big (and needs to be refined).  [triunsuitable x0
    y0 x1 y1 x2 y2 area] must return [true] if the triangle is too
    big. The arguments are as follow:
    - [x0] and [y0] are the X an Y coordinates of the triangle's origin vertex.
    - [x1] and [y2] are the X an Y coordinates of the triangle's
      destination vertex.
    - [x2] and [y2] are the X an Y coordinates of the triangle's apex vertex.
    - [area] is the area of the triangle.
*)

val triangulate :
  ?min_angle:float ->
  ?max_area:float ->
  ?convex_hull:bool ->
  ?max_steiner:int ->
  ?voronoi:bool ->
  ?edge:bool ->
  ?subparam:bool ->
  ?triangle_area:'a Mesh.vec ->
  ?debug:bool ->
  'a pslg -> 'a t * 'a voronoi
(** [triangulate pslg] returns a triangulation of the domain
    described by [pslg].

    If [pslg#segment] is empty, the convex hull of the set of points
    is used. FIXME: fails.

    @param debug if true, outputs some explanation of what Triangle is
    doing and some statistics.  Default: [true] as it can contain
    interesting information during program development. *)

val refine :
  ?min_angle:float ->
  ?max_area:float ->
  ?convex_hull:bool ->
  ?max_steiner:int ->
  ?voronoi:bool ->
  ?edge:bool ->
  ?subparam:bool ->
  ?triangle_area:'a Mesh.vec ->
  ?debug:bool ->
  'a t -> 'a t * 'a voronoi
