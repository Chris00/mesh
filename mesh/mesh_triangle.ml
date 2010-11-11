(*pp camlp4o pa_macro.cmo *)
(* File: triangle.ml

   Copyright (C) 2009

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

open Bigarray

external init : unit -> unit = "ocaml_triangle_init"
let () = init()

class type ['l] voronoi =
object
  inherit ['l] Mesh.voronoi
  method point_attribute : 'l Mesh.mat
end

type triunsuitable =
    float -> float -> float -> float -> float -> float -> float -> bool

let triunsuitable : triunsuitable -> unit =
  Callback.register "triunsuitable_callback"

module F =
struct
  type layout = fortran_layout
  external triangle :
    string ->                           (* options *)
    'l Mesh.t ->
    'l vec                              (* trianglearea *)
     -> 'l Mesh.t * 'l voronoi = "triangulate_fortran_layout"

  DEFINE NCOLS(a) = Array2.dim2 a;;
  DEFINE NROWS(a) = Array2.dim1 a;;
  DEFINE COLS = "dim2";;
  DEFINE ROWS = "dim1";;
  INCLUDE "mesh_triangleFC.ml";;
end

module C =
struct
  type layout = c_layout
  external triangle : string -> 'l Mesh.t -> 'l vec (* trianglearea *)
  -> 'l Mesh.t * 'l voronoi = "triangulate_c_layout"
  DEFINE NCOLS(a) = Array2.dim1 a;;
  DEFINE NROWS(a) = Array2.dim2 a;;
  DEFINE COLS = "dim1";;
  DEFINE ROWS = "dim2";;
  INCLUDE "mesh_triangleFC.ml";;
end

let invalid_arg m = invalid_arg ("Mesh.triangulate: " ^ m)

let triangulate ?point_attribute ?(refine=true) ?triangle_attribute
    ?min_angle ?max_area
    ?(convex_hull=false) ?max_steiner ?(voronoi=false) ?(edge=false)
    ?(subparam=false) ?triangle_area mesh =
  let layout = Array2.layout mesh.point in
  let dim1, dim2, triangle, switches, sdim1, sdim2 =
    if layout = (Obj.magic c_layout : 'a Bigarray.layout) then
      Array2.dim1, Array2.dim2, triangulate_c, "z", "dim1", "dim2"
    else
      Array2.dim2, Array2.dim1, triangulate_fortran, "", "dim2", "dim1" in

  (* Check points *)
  if dim2 mesh.point <> 2 then invalid_arg(sdim2 ^ " point <> 2");
  if dim2 mesh.point_attribute > 0
    && dim1 mesh.point_attribute < dim1 mesh.point then
      invalid_arg(sdim1 ^ " point_attribute < " ^ sdim1 ^ " point");
  if Array1.dim mesh.point_marker > 0
    && Array1.dim mesh.point_marker < dim1 mesh.point then
      invalid_arg("dim point_marker < " ^ sdim1 ^ " point");
  (* Check for PSLG *)
  let switches =
    if pslg then begin
      if dim1 mesh.segment > 0 then begin
        if dim2 mesh.segment <> 2 then invalid_arg(sdim2 ^ " segment <> 2");
        if Array1.dim mesh.segment_marker > 0
          && Array1.dim mesh.segment_marker < dim1 mesh.segment then
            invalid_arg("dim segment_marker < " ^ sdim1 ^ " segment");
      end;
      if not refine then begin
        if dim1 mesh.hole > 0 && dim2 mesh.hole <> 2 then
          invalid_arg(sdim2 ^ " hole <> 2");
        if dim1 mesh.region > 0 && dim2 mesh.region <> 4 then
          invalid_arg(sdim2 ^ " region <> 4");
      end;
      switches ^ "p"
    end else switches in
  (* Check for refinement -- triangles *)
  let switches =
    if refine then begin
      if dim1 mesh.triangle > 0 then begin
        if dim2 mesh.triangle <> 3 && dim2 mesh.triangle <> 6 then
          invalid_arg(sdim2 ^ " triangle must be 3 or 6");
        if dim2 mesh.triangle_attribute > 0
          && dim1 mesh.triangle_attribute < dim1 mesh.triangle then
            invalid_arg(sdim1 ^ " triangle_attribute < "
                        ^ sdim1 ^ " triangle");
      end;
      switches ^ "r"
    end else switches in
  (* Check triangle_area *)
  let switches, triangle_area = match triangle_area with
    | None -> switches, Array1.create prec layout 0
    | Some a ->
        if Array1.dim a < dim1 mesh.triangle then
          invalid_arg("dim triangle_area < " ^ sdim1 ^ " triangle");
        (switches ^ "a", a) in
  (* Other switches *)
  let switches = match min_angle with
    | None -> switches
    | Some a -> switches ^ "q" ^ (string_of_float a) in
  let switches = match max_area with
    | None -> switches
    | Some a -> switches ^ "a" ^ (string_of_float a) in
  let switches = if convex_hull then switches ^ "c" else switches in
  let switches = match max_steiner with
    | None -> switches
    | Some a -> switches ^ "S" ^ (string_of_int a) in
  let switches = if voronoi then switches ^ "v" else switches in
  let switches = if edge then switches ^ "e" else switches in
  let switches = if subparam then switches ^ "o2" else switches in
  triangle switches mesh triangle_area




(* Loading various formats *)



(* Save to triangle format *)
let save mesh filename =
  (* .node file *)
  let fh = open_out (filename ^ ".node") in
  close_out fh
  (* .ele file *)
  (* .poly file *)
  (* .edge file *)
  (* .neigh file *)
