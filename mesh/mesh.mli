(* Mesh.mli                       Time-stamp: <2008-06-12 22:25:37 trch>

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
(** Generic mesh structure to be used with various meshers.  It also
    defines some functions to help to display and design geometries.

    @version 0.7
    @author Christophe Troestler <Christophe.Troestler\@umh.ac.be>
*)

open Bigarray

(** {2 Mesh data format} *)

type 'layout vec = (float, float64_elt, 'layout) Array1.t
    (** Float vector (parametrized by the layout). *)
type 'layout mat = (float, float64_elt, 'layout) Array2.t
    (** Float matrix (parametrized by the layout). *)
type 'layout int_vec = (int, int_elt, 'layout) Array1.t
    (** Integer vector (parametrized by the layout). *)
type 'layout int_mat = (int, int_elt, 'layout) Array2.t
    (** Integer matrix (parametrized by the layout). *)

(** Planar Straight Line Graph datastructure. *)
class type ['layout] pslg =
object
  method point : 'layout mat
    (** Array of points coordinates (x,y).  It is of size 2 * n
        (fortran_layout) where n >= 3 is the number of points.  So the
        coordinates of point number [i] are [(point.{1,i},
        point.{2,i})]. *)
  method point_marker : 'layout int_vec
    (** Array of points markers.  Points inside the domain receive the
        marker 0, so assign markers [>= 1] to distinguish different
        parts of the boundary.  It must either be empty (in which case
        it is equivalent to all the markers being 1), or it must be of
        size n, where n is the number of points.  *)

  method segment : 'layout int_mat
    (** Array of segments endpoints; 2 int per segment.  Segments are
        edges whose presence in the triangulation is enforced (although
        each segment may be subdivided into smaller edges).  It is of
        size 2 * n (fortran_layout) for some n >= 0. *)
  method segment_marker : 'layout int_vec
    (** Array of segment markers.  It must either be empty (in which
        case it is equivalent to all the markers being 1), or it must be
        of size n, where n is the number of segments.  *)

  method hole : 'layout mat
  method region : 'layout mat
end

(** Object describing various caracteristics of a mesh.

    The two dimensional arrays are described for the [fortran_layout].
    If you use a [c_layout], they are transposed: for example [point]
    is of size n * 2 and the coordinates of point [i] are given by
    [(point.{i,0}, point.{i,1})]. *)
class type ['layout] t =
object
  inherit ['layout] pslg

  method triangle : 'layout int_mat
    (** Array of triangle corners: for each triangle, give the 3 corners
        followed by other nodes if the triangle represents a nonlinear
        element.  Its size is c * n (fortran_layout) where n > 0 is the
        number of triangles and c >= 3 is the number of nodes. *)
  method neighbor : 'layout int_mat
    (** Array of triangle neighbors; 3 int per triangle.  It is of size
        3 * n (fortran_layout) where n is 0 (i.e., neighbouring
        information is not given) or the number of triangles. *)
  method edge : 'layout int_mat
    (** Array of edge endpoints; 2 int per edge.  It is of size 2 * n
        (fortran_layout) where n > 0 is the number of edges. *)
  method edge_marker : 'layout int_vec
    (** Array of edge markers.  It must either be empty (in which case
        it is equivalent to all the markers being 1), or it must be of
        size n, where n is the number of edges.  *)
end

(** Voronoi diagram. *)
class type ['layout] voronoi =
object
  method point : 'layout mat
    (** Array of points coordinates (x,y).  It is of size 2 * n
        (fortran_layout) where n is the number of points.  So the
        coordinates of point number [i] are [(point.{1,i},
        point.{2,i})]. *)
  method edge  : 'layout int_mat
  method normal: 'layout mat
end


val is_c_layout : 'l pslg -> bool
  (** [is_c_layout] returns true if the mesh layout is C. *)


(** {2 Level curves} *)



(** {2 LaTeX output}

    The LaTex output is given in terms of three macros [\meshline],
    [\meshpoint], and [\meshtriangle] to plot edges, points and
    (filled) triangles.  The arguments of these macros are described
    by comments in the output.  If you do not provide your own
    implementations, default ones will be used.  *)

val latex : 'l t -> string -> unit
  (** [latex mesh file] saves the mesh as LaTeX PGF commands.  You can
      input the file in a tikzpicture environment to render the mesh.
      You will need to use the package "tikz" -- which works for
      PostScript as well as PDF output.  *)

val level_curves : ?boundary:(int -> string option) ->
  'l t -> 'l vec -> float list -> string -> unit
  (** [level_curves mesh z levels file] outputs into [file] LaTeX PGF
      commands to display the level curves at [levels] of the FEM
      surface with values [z] on the mesh [mesh].

      @param boundary specifies the color of the boundary edges given
      their marker value.  Returning [None] means that one does not
      want the border with that marker to be printed. *)


(** {2 Scilab} *)

val scilab : 'l t -> 'l vec -> string -> unit
  (** [scilab mesh z file] saves the mesh data and the function values
      [z] (i.e. [z.{i}] is the function value at the point
      [mesh.point.{_,i}] (fortran layout)) on that mesh so that when
      Scilab runs the created [file].sci script, the graph of the
      function is drawn. *)

(** {2 Matlab} *)

val matlab : 'l t -> 'l vec -> string -> unit
  (** [matlab mesh z file] saves the mesh data and the function values
      [z] (i.e. [z.{i}] is the function value at the point
      [mesh.point.{_,i}] (fortran layout)) on that mesh so that when
      Matlab runs the created [file].m script, the graph of the
      function is drawn. *)
