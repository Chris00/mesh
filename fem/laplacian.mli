(* File: laplacian.mli

   Copyright (C) 2008

     Christophe Troestler <Christophe.Troestler@umh.ac.be>
     WWW: http://math.umh.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

(** P1 functions on a mesh. *)

open Bigarray

type vec = (float, float64_elt, fortran_layout) Array1.t

type t

val make : ?bc: (int -> float -> float -> float option) ->
  fortran_layout Mesh.t -> t
  (** [make mesh] returns representation of the Laplacian on the [mesh].

      @param bc allows to define the boundary conditions (this it is
      only called for nodes with a non-zero [point_marker]).  [bc m x
      y] returns [None] if the node, of coordinates [(x,y)] and marker
      [m], must have Neumann boundary condition, and [Some v] if it
      must be equal to [v].  *)

val mesh : t -> fortran_layout Mesh.t
  (** [mesh lap] the mesh from which [lap] was created. *)

val area : t -> int -> float
  (** [area lap t] returns the area of the triangle nubered [t] in
      the underlying mesh. *)

val solve : t -> vec -> unit
  (** [solver lap b] solves the equation [- Laplacian(u) = b] and
      returns the answer in [b].  [b.{i}] is the coordinate in the FEM
      basis corresponding to the node [i].  If a Dirichlet boundary
      condition is imposed at node [i], the value of [b.{i}] will be
      ignored.  The solution satisfies the boundary conditions.  *)

val nonlin : ?y:vec -> t -> (float -> float -> float -> float) -> vec -> vec
  (** [nonlin ?y lap f] returns a function [fm] such that [fm u]
      computes the vector corresponding to the FEM weak form of the
      non-linearity [f x y u]. *)

val integrate : t -> (float -> float -> float -> float) -> (vec -> float)
  (** [integrate lap f u] integrate the function [f x y u] on the
      whole domain, where [u] is a function defined on the mesh. *)

val inner : t -> vec -> vec -> float
  (** [inner] returns the H^1_0 inner product on the mesh.  Note that
      at least one boundary point must have a Dirichlet boundary
      condition, otherwise this product is degenerate (on the
      constants). *)

val norm2 : t -> vec -> float
  (** [norm2 lap u] is the square norm of [u] (it is a shortcut for
      [inner lap u u]). *)

val norm : t -> vec -> float
  (** [norm lap u] is the norm of [u] (it is a shortcut for [sqrt(norm lap u]). *)

val pos : vec -> vec
  (** [pos u] returns the positive part of [u] (as a function),
      i.e. x -> max{u(x), 0}. *)

val neg : vec -> vec
  (** [neg u] returns the negative part of [u] (as a function),
      i.e. x -> min{u(x), 0}. *)
