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
open Lacaml.Impl.D

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

val nnodes : t -> int
  (** The number of nodes, including the boundary ones (the vectors
      must be of that size).  Note that for the dimension of the space
      one must substract the number of Dirichlet boundary nodes.  *)

val solve : t -> vec -> unit
  (** [solver lap b] solves the equation [- Laplacian(u) = b] and
      returns the answer in [b].  [b.{i}] is the coordinate in the FEM
      basis corresponding to the node [i], i.e. \int b \phi_i.  If a
      Dirichlet boundary condition is imposed at node [i], the value
      of [b.{i}] will be ignored.  The solution satisfies the boundary
      conditions.  *)

val integrate : t -> (float -> float -> float -> float) -> (vec -> float)
  (** [integrate lap f u] integrate the function [f x y u] on the
      whole domain, where [u] is a function defined on the mesh. *)

val nonlin : ?y:vec -> t -> (float -> float -> float -> float) -> vec -> vec
  (** [nonlin ?y lap f] returns a function [fm] such that [fm u]
      computes the vector corresponding to the FEM weak form of the
      non-linearity [f x y u]. *)

val hessian : ?y:mat -> t -> (float -> float -> float -> float) -> (vec -> mat)
  (** [hessian kap f u] returns the matrix of the Hessian assotiated
      to [f], i.e. [int f(x,u) \phi_i \phi_j] *)

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

val dual : t -> ?y:vec -> vec -> vec
  (** [dual lap u] is the vector [inner lap u phi_i] where [phi_i]
      runs through all the finite elements basis (including boundary
      nodes). *)

val add_inner : ?alpha:float -> t -> mat -> unit
  (** [add_inner lap m] adds the inner product matrix to [m]. *)

val solve_with_bc : t -> mat -> vec -> unit
  (** [solve lap m v] solves the system [m * u = v] and store the
      solution in [v].  The solution will satisfy the boundary
      conditions of the space (only the equations that correspond to
      non-Dirichlet boundary conditions are considered).  WARNING: the
      matrix [m] is modified. *)

val vec_of_fun : t -> (float -> float -> float) -> vec
  (** [vec_of_fun u] returns the P1 interpolation of the function [fun x
      y -> u x y].  Regardless of whether [u] has the right values at
      boundary points with Dirichlet boundary conditions, these are
      enforced in the returned vector. *)

val pos : vec -> unit
  (** [pos u] project [u] on its positive part (as a function), i.e. x
      -> max{u(x), 0}. *)

val neg : vec -> unit
  (** [neg u] project [u] on its negative part (as a function), i.e. x
      -> min{u(x), 0}. *)
