(* File: mesh_display.ml

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


(** Displaying the mesh with Graphics *)

open Printf
open Bigarray
open Graphics
open Mesh


(* TODO:

   - allow to zoom in and out
   - display the segments in another color
   - allow to switch on and off the nodes #
*)

module F =
struct
  type mesh = fortran_layout t;;
  type 'a vector = 'a vec               (* global vec *)
  type vec = fortran_layout vector;;  (* local vec *)
  DEFINE NCOLS(a) = Array2.dim2 a;;
  DEFINE NROWS(a) = Array2.dim1 a;;
  DEFINE FST = 1;;
  DEFINE SND = 2;;
  DEFINE THIRD = 3;;
  DEFINE LASTCOL(a) = Array2.dim2 a;;
  DEFINE GET(a,i,j) = a.{i,j};;
  INCLUDE "mesh_displayFC.ml";;
end

module C =
struct
  type mesh = c_layout t;;
  type 'a vector = 'a vec               (* global vec *)
  type vec = c_layout vector;;
  DEFINE NCOLS(a) = Array2.dim1 a;;
  DEFINE NROWS(a) = Array2.dim2 a;;
  DEFINE FST = 0;;
  DEFINE SND = 1;;
  DEFINE THIRD = 2;;
  DEFINE LASTCOL(a) = Array2.dim1 a - 1;;
  DEFINE GET(a,i,j) = a.{j,i};;
  INCLUDE "mesh_displayFC.ml";;
end


let draw ?width ?height ?color ?voronoi ?point_marker_color ?segments
    (mesh: 'a #t) =
  if Mesh.is_c_layout(mesh :> _ Mesh.pslg) then
    C.draw ?width ?height ?color ?voronoi ?point_marker_color ?segments
      (Obj.magic mesh)
  else
    F.draw ?width ?height ?color ?voronoi ?point_marker_color ?segments
      (Obj.magic mesh)

let init_graph width height =
  let xbd = 10 and ybd = 10 in
  (* Drawing itself *)
  open_graph (sprintf " %ix%i-40+40" (width + 2 * xbd) (height + 2 * ybd));
  moveto xbd ybd

let hold_graph () =
  (* Wait for the key 'q' to be pressed. *)
  try
    while true do
      let status = wait_next_event [Button_down; Key_pressed] in
      if status.button
        || (status.keypressed && (status.key = 'q' || status.key = 'Q')) then (
        close_graph();
        raise Exit
      );
    done
  with Exit -> ()

let display ?(width=600) ?(height=600) ?color ?voronoi ?point_marker_color
    ?(segments=true) mesh =
  init_graph width height;
  set_window_title("Mesh (" ^ Filename.basename Sys.argv.(0) ^ ")");
  draw ~width ~height ?color ?voronoi ?point_marker_color ~segments mesh;
  hold_graph()

let level_curves ?(width=600) ?(height=600) ?boundary (mesh: 'a #t) (z: 'a vec)
    ?level_eq levels =
  if Mesh.is_c_layout(mesh :> _ Mesh.pslg) then
    C.level_curves ~width ~height ?boundary
      (Obj.magic mesh) (Obj.magic z) ?level_eq levels
  else
    F.level_curves ~width ~height ?boundary
      (Obj.magic mesh) (Obj.magic z) ?level_eq levels

let display_level_curves ?(width=600) ?(height=600) ?boundary mesh z
    ?level_eq levels =
  init_graph width height;
  set_window_title("Level curves (" ^ Filename.basename Sys.argv.(0) ^ ")");
  level_curves ~width ~height ?boundary mesh z ?level_eq levels;
  hold_graph()
