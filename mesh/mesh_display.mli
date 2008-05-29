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
  ?width:int -> ?height:int -> ?voronoi:'a Mesh.voronoi -> ?segments:bool ->
  'a Mesh.t -> unit
  (** [draw mesh] display the mesh on the current OCaml Graphics
      window with the bottom left corner at the current position.  The
      mesh is drawn with the current color.

      @param width the width (in pixels) of the mesh image.
      @param height the height (in pixels) of the mesh image.
      @param voronoi draw also the voronoi diagram given
  *)

val display :
  ?width:int -> ?height:int -> ?voronoi:'a Mesh.voronoi -> ?segments:bool ->
  'a Mesh.t -> unit
  (** [display mesh] open an OCaml graphic window and draw the mesh on
      it.  *)
