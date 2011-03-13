(*pp camlp4o pa_macro.cmo *)
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

open Bigarray
open Printf
open Scanf
open Mesh

class ['l] pslg = ['l] Mesh.pslg

module F =
struct
  type pslg = fortran_layout Mesh.pslg;;
  type mesh = fortran_layout Mesh.t;;
  DEFINE LAYOUT = fortran_layout;;
  DEFINE NCOLS(a) = Array2.dim2 a;;
  DEFINE CREATE_MAT(kind,n,m) = Array2.create kind LAYOUT n m;;
  DEFINE CREATE_VEC(kind,n) = Array1.create kind LAYOUT n;;
  DEFINE FST = 1;;
  DEFINE SND = 2;;
  DEFINE THIRD = 3;;
  DEFINE LASTCOL(a) = Array2.dim2 a;;
  DEFINE GET(a,i,j) = a.{i,j};;
  DEFINE OF_IDX(i) = i - 1;;            (* Fortran -> Easymesh indexes *)
  DEFINE TO_IDX(i) = i + 1;;            (* Easymesh -> Fortran *)
  INCLUDE "easymeshFC.ml";;
end

module C =
struct
  type pslg = c_layout Mesh.pslg;;
  type mesh = c_layout Mesh.t;;
  DEFINE LAYOUT = c_layout;;
  DEFINE NCOLS(a) = Array2.dim1 a;;
  DEFINE CREATE_MAT(kind,n,m) = Array2.create kind LAYOUT m n;;
  DEFINE CREATE_VEC(kind,n) = Array1.create kind LAYOUT n;;
  DEFINE FST = 0;;
  DEFINE SND = 1;;
  DEFINE THIRD = 2;;
  DEFINE LASTCOL(a) = Array2.dim1 a - 1;;
  DEFINE GET(a,i,j) = a.{j,i};;
  DEFINE OF_IDX(i) = i;;
  DEFINE TO_IDX(i) = i;;
  INCLUDE "easymeshFC.ml";;
end

(* Interface to easymesh -- gathering the FORTRAN and C layouts together
 ***********************************************************************)

(* BEWARE that the result type of [read] must be in accordance with
   the layout. *)
let read_mesh (pslg : 'a Mesh.pslg) fname : 'a Mesh.t =
  if is_c_layout pslg then
    Obj.magic(C.read (Obj.magic pslg : c_layout Mesh.pslg) fname)
  else
    Obj.magic(F.read (Obj.magic pslg : fortran_layout Mesh.pslg) fname)


let read layout fname : 'a Mesh.t =
  if layout = (Obj.magic Bigarray.c_layout : 'a layout)
  then Obj.magic(read_mesh C.empty_pslg fname : c_layout Mesh.t)
  else Obj.magic(read_mesh F.empty_pslg fname : fortran_layout Mesh.t)

let triangulate ~max_area (pslg: 'a Mesh.pslg) =
  (* Save domain file *)
  let (fname_plsg, fh) = Filename.open_temp_file "EasyMesh" ".d" in
  let fname = Filename.chop_extension fname_plsg in
  if is_c_layout pslg then
    C.output_pslg fh (Obj.magic pslg) max_area
  else
    F.output_pslg fh (Obj.magic pslg) max_area;
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

let write (mesh: _ #Mesh.t) file =
  if is_c_layout (mesh :> _ Mesh.pslg) then
    C.write (Obj.magic mesh : c_layout Mesh.t) file
  else
    F.write (Obj.magic mesh : fortran_layout Mesh.t) file
