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
  'a Mesh.t -> unit
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
  'a Mesh.t -> unit
  (** [display mesh] open an OCaml graphic window and draw the mesh on
      it.  See {!Mesh_display.draw} for the meaning of the optional
      parameters. *)
