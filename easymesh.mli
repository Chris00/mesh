(* Easymesh.mli                    Time-stamp: <2006-08-23 18:47:26 trch>

  Copyright (C) 2001-2004

     Christophe Troestler
     email: Christophe.Troestler@umh.ac.be
     WWW: http://www.umh.ac.be/math/an/software/

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details; it is available at
   <http://www.fsf.org/copyleft/gpl.html>, or by writing to the Free
   Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.
*)
(**
   OCaml interface for EasyMesh.

   {{:http://www-dinma.univ.trieste.it/nirftc/research/easymesh/}EasyMesh}
   is a simple and easy to use mesh generator.  However, it is not
   very robust and can segfault instead of reporting an error.

   @version 0.5
   @author Christophe Troestler (Christophe.Troestler(at)umh.ac.be)
*)


val triangulate : max_area:float -> 'layout Mesh.t -> 'layout Mesh.t
  (** [triangulate ~max_area pslg] returns a triangulation of the
      Planar Straight Line Graph [pslg] given by [pslg.Mesh.points]
      and [pslg.Mesh.segment].  BEWARE that for EasyMesh, the boundary
      must have a positive (counterclockwise) orientation, holes must
      be delimited by a negatively orientated paths.

      [pslg.Mesh.points_marker] and [pslg.Mesh.segment_marker] may be set.

      @param max_area is given as an indication to the algorithm and
      may not be respected. *)

val read : 'layout Bigarray.layout -> string -> 'layout Mesh.t
  (** [read layout file] reads the mesh described by the files [file].n,
      [file].e and [file].s into a Mesh.t structure.  Only the fact that
      the files are well formed is checked (various exceptions may be
      thrown), not the fact that the data describe a real mesh. *)
