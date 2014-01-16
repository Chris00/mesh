(* File: easymesh.ml

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

(** Put the full path if not in your search path. *)
let easymesh = "EasyMesh"

open Printf
open Bigarray
open Mesh_common

class ['l] pslg = ['l] Mesh_common.pslg


(* Interface to easymesh -- gathering the FORTRAN and C layouts together
 ***********************************************************************)

(* BEWARE that the result type of [read] must be in accordance with
   the layout. *)
let read_mesh (pslg : 'a Mesh.pslg) fname : 'a t =
  if is_c_layout pslg then
    Obj.magic(EasymeshC.read (pslg_to_c pslg) fname)
  else
    Obj.magic(EasymeshF.read (pslg_to_fortran pslg) fname)


let read layout fname : 'a Mesh.t =
  if Mesh_utils.is_c_layout layout
  then Obj.magic(read_mesh EasymeshC.empty_pslg fname : c_layout t)
  else Obj.magic(read_mesh EasymeshF.empty_pslg fname : fortran_layout t)

let triangulate ~max_area (pslg: 'a pslg) =
  (* Save domain file *)
  let (fname_plsg, fh) = Filename.open_temp_file "EasyMesh" ".d" in
  let fname = Filename.chop_extension fname_plsg in
  if is_c_layout pslg then
    EasymeshC.output_pslg fh (pslg_to_c pslg) max_area
  else
    EasymeshF.output_pslg fh (pslg_to_fortran pslg) max_area;
  close_out fh;
  (* Execute easymesh *)
  let _ = Sys.command (sprintf "%s %s -m" easymesh fname) in
  (* The return code of EasyMesh is unrelialble, do not check it. *)
  (* Read the result *)
  let mesh = read_mesh pslg fname in
  Sys.remove (fname_plsg);
  Sys.remove (fname ^ ".n");
  Sys.remove (fname ^ ".e");
  Sys.remove (fname ^ ".s");
  mesh

let write (mesh: _ #t) file =
  if is_c_layout mesh then EasymeshC.write (mesh_to_c mesh) file
  else EasymeshF.write (mesh_to_fortran mesh) file
