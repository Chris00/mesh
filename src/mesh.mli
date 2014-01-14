(* Mesh.mli

  Copyright (C) 2001-2011

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

    General remark: The two dimensional arrays below are described for
    the [fortran_layout].  If you use a [c_layout], they are
    transposed.  Also indices, such as the indices points,
    triangles,..., start at [0] instead of [1] .  For example
    [mesh#point] is of size [n * 2] and the coordinates of point [i]
    are given by [(point.{i,0}, point.{i,1})].

    @author Christophe Troestler (Christophe.Troestler\@umons.ac.be)
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

(** Planar Straight Line Graph datastructure.  By default, creating an
    object from this class results in all methods being initialized
    with empty arrays. *)
class ['l] pslg : 'l layout ->
object
  method point : 'l mat
  (** Array of points coordinates (x,y).  It is of size [2 * n]
      ([fortran_layout]) where [n >= 3] is the number of points.  So the
      coordinates of point number [i] are [(point.{1,i}, point.{2,i})].
  *)
  method point_marker : 'l int_vec
  (** Array of points markers.  Points inside the domain receive the
      marker [0], so assign markers [>= 1] to distinguish different
      parts of the boundary.  It must either be empty (in which case
      it is equivalent to all the markers being [1]), or it must be of
      size [n], where [n > 0] is the number of points.  *)

  method segment : 'l int_mat
  (** Array of segments endpoints; 2 int per segment (the indices of
      the corresponding points).  Segments are edges whose presence
      in the triangulation is enforced (although each segment may be
      subdivided into smaller edges).  It is of size [2 * n]
      ([fortran_layout]) for some [n >= 0]. *)
  method segment_marker : 'l int_vec
  (** Array of segment markers.  It must either be empty (in which
      case it is equivalent to all the markers being [1]), or it must be
      of size [n], where [n] is the number of segments.  *)

  method hole : 'l mat
  (** Array of holes.  For each hole, the array specifies a point (x,y)
      inside the hole.  It is of size [2 * n] ([fortran_layout]) where
      [n >= 0] is the number of wholes. *)

  method region : 'l mat
  (** Array of regional attributes and area constraints.  It is of
      size [4 * n] ([fortran_layout]) where [n >= 0] is the number of
      regions.  For a region [i], [region{1,i}] and [region{2,i}] are
      the x and y coordinates of a point inside the region (the region
      is bounded by segments), [region{3,i}] is the regional
      attribute, and [region{4,i}] is the maximum area.  If you wish to
      specify a regional attribute but not a maximum area for a given
      region, set [region{4,i}] to a negative value. *)
end

(** Object describing various characteristics of a mesh. *)
class type ['layout] t =
object
  inherit ['layout] pslg

  method triangle : 'layout int_mat
  (** Array of triangle corners: for each triangle, give the 3 indices
      of its corners, followed by (the indices of) other nodes if the
      triangle represents a nonlinear element.  Its size is [c * n]
      ([fortran_layout]) where [n > 0] is the number of triangles and
      [c >= 3] is the number of nodes. *)
  method neighbor : 'layout int_mat
  (** Array of triangle neighbors; 3 int per triangle.  It is of size
      [3 * n] ([fortran_layout]) where [n] is 0 (i.e., neighbouring
      information is not given) or the number of triangles.  Negative
      entries must be skipped (boundary triangles have less than 3
      neighbors).  *)
  method edge : 'layout int_mat
  (** Array of edge endpoints; 2 int per edge.  It is of size [2 * n]
      ([fortran_layout]) where [n > 0] is the number of edges. *)
  method edge_marker : 'layout int_vec
  (** Array of edge markers.  Edges inside the domain receive the
      marker [0].  It must either be empty (meaning that the
      information is not provided) or it must be of size [n], where
      [n] is the number of edges.  *)
end

(** Voronoi diagram. *)
and ['layout] voronoi =
object
  method point : 'layout mat
  (** Array of points coordinates (x,y).  It is of size [2 * n]
      ([fortran_layout]) where [n] is the number of points.  So the
      coordinates of point number [i] are [(point.{1,i}, point.{2,i})]. *)
  method edge  : 'layout int_mat
  (** Array of edge endpoints; 2 int per edge.  It is of size [2 * n]
      ([fortran_layout]) where [n > 0] is the number of edges. *)
  method normal: 'layout mat
  (** Array of normal vectors, used for infinite rays in the Voronoi
      diagram.  It is of dimensions [2 * n] ([fortran_layout]) for
      some [n] is the number of edges.  The normal vector for edge [i]
      is given by [(normal.{1,i}, normal.{2,i})].  It the edge in the
      Voronoi diagram is finite, the normal vector is zero. *)
end

class ['l] alias : 'l #t -> ['l] t
(** [alias mesh] creates a new object that shares its matrices with
    [mesh].  This is especially useful if you want to extend a mesh
    object trough inheritance. *)

val layout : 'l #pslg -> 'l layout
(** [layout mesh] returns the layout of [mesh]. *)

val is_c_layout : 'l #pslg -> bool
(** [is_c_layout] returns true if the mesh layout is C. *)


(** {2:band  Band computation and reduction} *)

val band_height_P1 : ?filter:(int -> bool) -> 'l #t -> int
(** [band_height mesh] returns the number of nonzero super-diagonals +
    1 (for the diagonal) of symmetric band matrices for P1 finite
    elements inner products.  It is the maximum on all triangles T of
    max(|i1 - i2|, |i2 - i3|, |i3 - i1|) where i1, i2, and i3 are
    the indices of the nodes of the three corners of the triangle T.

    @param filter If provided, only perform the compuation on nodes
    [i] such that [filter i] is true.  Default: no filter. *)

val cuthill_mckee : ?rev:bool -> ?perm:'l int_vec -> 'l #t -> 'l t
(** [cuthill_mckee mesh] return a new mesh that is identical to [mesh]
    except that the labelling of the nodes has been changed to lower
    its band (as computed by {!band_height_P1}).

    @param rev whether if true, use the Reverse CutHill-McKee algorithm.
    Default: [true].

    @param perm if provided, the permutation will be stored in that
    vector.  More precisely, [perm.{l} = i] means that [l] is the new
    label for the node initially labeled [i].  This permutation is
    needed to transfer vectors defined on the initial labeling.  The
    length of the permutation vector must be the number of nodes. *)

val permute_points : ?inv:bool -> 'l int_vec -> 'l #t -> 'l t
(** [permute_points p mesh] returns a new mesh identical to the given
    [mesh] except that the points indices are transformed through the
    permutation [p]: the point of index [i] in the new mesh will be
    the one of index [p.{i}] in [mesh].  In other words, [p] lists
    [mesh] indices in the order they will have after permutation.

    @raise Invalid_argument if [p] is not a permutation.
    @param inv consider that the inverse permutation is given.  If
    [true], the point of index [i] in [mesh] will be mapped to the
    point of index [p.{i}] in the returned mesh.  Default: [false]. *)


(** {2 LaTeX output} *)

(** LaTex output.  It is given in terms of three macros [\meshline],
    [\meshpoint], and [\meshtriangle] to plot edges, points and
    (filled) triangles.  The arguments of these macros are described
    by comments in the output files.  If you do not provide your own
    implementations, default ones will be used.  The LaTeX package
    {{:https://sourceforge.net/projects/pgf/}tikz} is required --
    which works for PostScript as well as PDF output. *)
module LaTeX :
sig
  type color = int
  (** RGB Color representation [0xRRGGBB].  It will be converted to
      TeX by the functions below. *)

  val save : ?edge:(int -> color option) -> 'l #t -> string -> unit
  (** [save mesh file] saves the mesh as LaTeX PGF commands.  You can
      input the file in a [tikzpicture] environment to render the mesh.

      @param edge allows to specify the color of each edge.  If the
      function returns [None], the edge is not drawn.  Default: all
      are black. *)

  val level_curves : ?boundary:(int -> color option) ->
    'l #t -> 'l vec -> ?level_eq:(float -> float -> bool) ->
    (float * color) list -> string -> unit
    (** [level_curves mesh z levels file] outputs into [file] LaTeX
        PGF commands to display the level curves at [levels] of the P1
        FEM surface with values [z] on the mesh [mesh].  Each level is
        a couple [(l, c)] where [l] is the level value and [c] is the
        RGB color to be used to display it. The output is done as TeX
        macros [\meshline{R,G,B}{x1}{y1}{x2}{y2}], [\meshpoint{point
        number}{x}{y}] and
        [\meshtriangle{R,G,B}{x1}{y1}{x2}{y2}{x3}{y3}], so it is
        easily customizable from within LaTeX.  Default values for
        these macros are provided if they are not defined.

        @param boundary specifies the color of the boundary edges given
        their marker value.  Returning [None] means that one does not
        want the border with that marker to be printed.

        @param level_eq an approximate equality for levels that are
        judged not to be distinguishable.  It is expected that [l1 =
        l2] implies [level_eq l1 l2].  This function is mainly used
        not to draw the boundary edges at levels given in
        [levels].  *)

  val super_level : ?boundary:(int -> color option) ->
    'l #t -> 'l vec -> float -> color -> string -> unit
  (** [super_level mesh z level color file] outputs into [file] the
      LaTeX PGF command to display the super-level \{ (x,y) | z(x,y) >
      l \}.  You can customize the output by defining
      [\meshfilltriangle{R,G,B}{x1}{y1}{x2}{y2}{x3}{y3}] and
      [\meshfillquadrilateral{R,G,B}{x1}{y1}{x2}{y2}{x3}{y3}{x4}{y4}].  *)

  val sub_level : ?boundary:(int -> color option) ->
    'l #t -> 'l vec -> float -> color -> string -> unit
  (** [sub_level mesh z level color file] same as {!super_level}
      except that the sub-level \{ (x,y) | z(x,y) < l \} is drawn. *)
end

(** {2 Scilab} *)

val scilab : 'l #t -> 'l vec -> string -> unit
  (** [scilab mesh z file] saves the mesh data and the function values
      [z] (i.e. [z.{i}] is the function value at the point
      [mesh.point.{_,i}] ([fortran layout])) on that mesh so that when
      Scilab runs the created [file].sci script, the graph of the
      function is drawn. *)

(** {2 Matlab} *)

val matlab : 'l #t -> ?edgecolor:string -> ?linestyle:string -> ?facealpha:float ->
             'l vec -> string -> unit
  (** [matlab mesh z file] saves the mesh data and the function values
      [z] (i.e. [z.{i}] is the function value at the point
      [mesh.point.{_,i}] ([fortran layout])) on that mesh so that when
      Matlab runs the created [file].m script, the graph of the
      function is drawn.

      @param edgecolor the name of the color for the edges of the
      triangles.  Default: ["black"].  The special value ["none"]
      removes them.

      @param linestyle is a symbol representing a style of line:
      ["-"] is a solid line, ["--"] a dashed line, [":"] a dotted line,
      ["-."] a dash-dot line and ["none"] means that no line is drawn.
      Default: ["-"].

      @param facealpha the transparency of faces, [0.] meaning fully
      transparent and [1.] opaque.  Default: [1.]. *)

(** {2 Mathematica} *)

val mathematica : 'l #t -> 'l vec -> string -> unit
(** [mathematica mesh z file] saves the mesh data and the function
    values [z] (i.e. [z.{i}] is the function value at the point
    [mesh.point.{_,i}] ([fortran layout])) in the file [file].m so
    that running "<<file.m" in mathematica plots the function.  The
    variables [File`xyz] and [File`adj] (where [File] is the
    capitalized file name which is used as the context) are defined
    and the mathematica command [TriangularSurfacePlot[File`xyz,
    File`adj]] does the plot.

    If [file] contains other digits than alphanumeric
    (e.g. underscores), they are removed for the context names
    [File] used for internal variables. *)
