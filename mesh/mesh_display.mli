(* File: mesh_display.mli

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

val draw :
  ?width:int -> ?height:int -> ?color: int -> ?voronoi:'a Mesh.voronoi ->
  ?point_marker_color: int -> ?segments:bool ->
  'a #Mesh.t -> unit
  (** [draw mesh] display the mesh on the current OCaml Graphics
      window with the bottom left corner at the current position.

      @param width the width (in pixels) of the mesh image (default: 600).
      @param height the height (in pixels) of the mesh image (default: 600).
      @param color the color to draw the mesh (default: the foreground color).
      @param voronoi draw also the given voronoi diagram.
      @param point_marker_color trigger the display of the point
      markers with the color given (default: no markers). *)

val display :
  ?width:int -> ?height:int -> ?color: int -> ?voronoi:'a Mesh.voronoi ->
  ?point_marker_color: int -> ?segments:bool ->
  'a #Mesh.t -> unit
  (** [display mesh] open an OCaml graphic window and draw the mesh on
      it.  See {!Mesh_display.draw} for the meaning of the optional
      parameters. *)

val level_curves : ?width:int -> ?height:int ->
  ?boundary:(int -> Graphics.color option) -> 'l #Mesh.t -> 'l Mesh.vec ->
  ?level_eq:(float -> float -> bool) -> (float * Graphics.color) list -> unit
  (** [level_curves mesh z levels] display a graphics window with the
      requested level curves.  Each level is a couple [(l, c)] where
      [l] is the lavel value and [c] is the color to be used to
      display it.

      @param boundary specifies the color of the boundary edges given
      their marker value.  Returning [None] means that one does not
      want the border with that marker to be printed.

      @param level_eq an approximate equality for levels that are judged not
      to be distinguishable.  It is expected that [l1 = l2] implies
      [level_eq l1 l2].  This function is mainly used not to draw
      the boundary edges at levels given in [levels].  *)
