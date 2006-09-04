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

(* Interface to easymesh for fortran_layout matrices
 ***********************************************************************)

open Bigarray
open Printf
open Mesh

(* Write the [pslg] to the channel [fh]. *)
let output_pslg_fortran fh (pslg: fortran_layout Mesh.t) area =
  let pt = pslg.point
  and seg = pslg.segment
  and pt_marker = pslg.point_marker
  and seg_marker = pslg.segment_marker in
  let pt_marker =
    if Array1.dim pt_marker > 0 then (fun i -> pt_marker.{i})
    else (fun i -> 1) in
  let seg_marker =
    if Array1.dim seg_marker > 0 then (fun i -> seg_marker.{i})
    else (fun i -> 1) in
  (* Save points coordinates *)
  fprintf fh "%i\n" (Array2.dim2 pt); (* number of nodes *)
  for i = 1 to Array2.dim2(pt) do
    (* EasyMesh expects indexes in 0 .. nnodes-1 *)
    fprintf fh "%i: %.13g %.13g %.13g %i\n"
      (i - 1) pt.{1,i} pt.{2,i}
      area (pt_marker i)
  done;
  (* Save segments *)
  fprintf fh "%i\n" (Array2.dim2 seg); (* number of segments *)
  for i = 1 to Array2.dim2(seg) do
    fprintf fh "%i: %i %i %i\n"
      (i - 1)
      (seg.{1,i} - 1) (seg.{2,i} - 1)
      (seg_marker i)
  done


(* FIXME: comments are possible in the files *)
(* FIXME: if a file does not exists, return empty array? *)
(* [read_fortran fname] reads the collection of filenames [fname].n,
   [fname].e and [fname].s and creates a mesh struture with fortran
   layout.  This function can throw a variety of exceptions depending
   of what goes wrong.  *)
let read_fortran fname =
  (* Read nodes *)
  let fh = open_in (fname ^ ".n") in
  let nnodes = int_of_string(input_line fh) in
  let pt = Array2.create float64 fortran_layout 2 nnodes in
  let pt_mark = Array1.create int fortran_layout nnodes in
  for i = 1 to Array2.dim2(pt) do
    Scanf.fscanf fh " %i: %g %g %i"
      (fun i x y m ->
         let i = (i + 1) in
         pt.{1,i} <- x;
         pt.{2,i} <- y;
         pt_mark.{i} <- m)
  done;
  close_in fh;
  (* Read triangles *)
  let fh = open_in (fname ^ ".e") in
  let n = int_of_string(input_line fh) in
  let tr = Array2.create int fortran_layout 3 n
  and tr_nb = Array2.create int fortran_layout 3 n in
  for i = 1 to Array2.dim2(tr) do
    Scanf.fscanf fh " %i: %i %i %i %i %i %i %_i %_i %_i %_f %_f %_i"
      (fun e i j k ei ej ek ->
         let e = (e + 1) in
         tr.{1,e} <- (i + 1);
         tr.{2,e} <- (j + 1);
         tr.{3,e} <- (k + 1);
         tr_nb.{1,e} <- (ei + 1);
         tr_nb.{2,e} <- (ej + 1);
         tr_nb.{3,e} <- (ek + 1);
      )
  done;
  close_in fh;
  try
    (* Read edges, if file exists *)
    let fh = open_in (fname ^ ".s") in
    let n = int_of_string(input_line fh) in
    let edge = Array2.create int fortran_layout 2 n
    and edge_marker = Array1.create int fortran_layout n in
    for i = 1 to Array2.dim2(edge) do
      Scanf.fscanf fh " %i: %i %i %_i %_i %i"
        (fun s c d m ->
           let s = (s + 1) in
           edge.{1,s} <- (c + 1);
           edge.{2,s} <- (d + 1);
           edge_marker.{s} <- m;
        )
    done;
    close_in fh;
    { (Mesh.empty fortran_layout) with
        point = pt;
        point_marker = pt_mark;
        triangle = tr;
        neighbor = tr_nb;
        edge = edge;
        edge_marker = edge_marker;
    }
  with Sys_error _ ->
    { (Mesh.empty fortran_layout) with
        point = pt;
        point_marker = pt_mark;
        triangle = tr;
        neighbor = tr_nb;
    }

(* Interface to easymesh for c_layout matrices
 ***********************************************************************)

open Bigarray
open Printf
open Mesh

(* Write the [pslg] to the channel [fh]. *)
let output_pslg_c fh (pslg: c_layout Mesh.t) area =
  let pt = pslg.point
  and seg = pslg.segment
  and pt_marker = pslg.point_marker
  and seg_marker = pslg.segment_marker in
  let pt_marker =
    if Array1.dim pt_marker > 0 then (fun i -> pt_marker.{i})
    else (fun i -> 1) in
  let seg_marker =
    if Array1.dim seg_marker > 0 then (fun i -> seg_marker.{i})
    else (fun i -> 1) in
  (* Save points coordinates *)
  fprintf fh "%i\n" (Array2.dim1 pt); (* number of nodes *)
  for i = 0 to Array2.dim1(pt) - 1 do
    (* EasyMesh expects indexes in 0 .. nnodes-1 *)
    fprintf fh "%i: %.13g %.13g %.13g %i\n"
      (i) pt.{i,0} pt.{i,1}
      area (pt_marker i)
  done;
  (* Save segments *)
  fprintf fh "%i\n" (Array2.dim1 seg); (* number of segments *)
  for i = 0 to Array2.dim1(seg) - 1 do
    fprintf fh "%i: %i %i %i\n"
      (i)
      (seg.{i,0}) (seg.{i,1})
      (seg_marker i)
  done


(* FIXME: comments are possible in the files *)
(* FIXME: if a file does not exists, return empty array? *)
(* [read_c fname] reads the collection of filenames [fname].n,
   [fname].e and [fname].s and creates a mesh struture with c
   layout.  This function can throw a variety of exceptions depending
   of what goes wrong.  *)
let read_c fname =
  (* Read nodes *)
  let fh = open_in (fname ^ ".n") in
  let nnodes = int_of_string(input_line fh) in
  let pt = Array2.create float64 c_layout nnodes 2 in
  let pt_mark = Array1.create int c_layout nnodes in
  for i = 0 to Array2.dim1(pt) - 1 do
    Scanf.fscanf fh " %i: %g %g %i"
      (fun i x y m ->
         let i = (i) in
         pt.{i,0} <- x;
         pt.{i,1} <- y;
         pt_mark.{i} <- m)
  done;
  close_in fh;
  (* Read triangles *)
  let fh = open_in (fname ^ ".e") in
  let n = int_of_string(input_line fh) in
  let tr = Array2.create int c_layout n 3
  and tr_nb = Array2.create int c_layout n 3 in
  for i = 0 to Array2.dim1(tr) - 1 do
    Scanf.fscanf fh " %i: %i %i %i %i %i %i %_i %_i %_i %_f %_f %_i"
      (fun e i j k ei ej ek ->
         let e = (e) in
         tr.{e,0} <- (i);
         tr.{e,1} <- (j);
         tr.{e,2} <- (k);
         tr_nb.{e,0} <- (ei);
         tr_nb.{e,1} <- (ej);
         tr_nb.{e,2} <- (ek);
      )
  done;
  close_in fh;
  try
    (* Read edges, if file exists *)
    let fh = open_in (fname ^ ".s") in
    let n = int_of_string(input_line fh) in
    let edge = Array2.create int c_layout n 2
    and edge_marker = Array1.create int c_layout n in
    for i = 0 to Array2.dim1(edge) - 1 do
      Scanf.fscanf fh " %i: %i %i %_i %_i %i"
        (fun s c d m ->
           let s = (s) in
           edge.{s,0} <- (c);
           edge.{s,1} <- (d);
           edge_marker.{s} <- m;
        )
    done;
    close_in fh;
    { (Mesh.empty c_layout) with
        point = pt;
        point_marker = pt_mark;
        triangle = tr;
        neighbor = tr_nb;
        edge = edge;
        edge_marker = edge_marker;
    }
  with Sys_error _ ->
    { (Mesh.empty c_layout) with
        point = pt;
        point_marker = pt_mark;
        triangle = tr;
        neighbor = tr_nb;
    }

(* Interface to easymesh -- gathering the FORTRAN and C layouts together
 ***********************************************************************)

(* Put the full path if not in your search path. *)
let easymesh = "EasyMesh"

open Printf


(* BEWARE that the result type of [read] must be in accordance with
   the layout. *)
let read (layout : 'a Bigarray.layout) fname : 'a Mesh.t =
  if layout = (Obj.magic Bigarray.c_layout : 'a Bigarray.layout) then
    Obj.magic (read_c fname)
  else
    Obj.magic (read_fortran fname)


let triangulate ~max_area (pslg: 'a Mesh.t) =
  (* Save domain file *)
  let (fname, fh) = Filename.open_temp_file "EasyMesh" ".d" in
  let fname = Filename.chop_extension fname in
  output_string fh "# EasyMesh domain file generated by OCaml Mesh module #\n";
  if is_c_layout pslg then
    output_pslg_c fh (Obj.magic (pslg : 'a t)) max_area
  else
    output_pslg_fortran fh (Obj.magic (pslg : 'a t)) max_area;
  close_out fh;
  (* Execute easymesh *)
  let err = Sys.command (sprintf "%s %s -m" easymesh fname) in
(*   if err <> 0 then *)
(*     failwith(sprintf "Easymesh.triangulate: %s returned status %i" *)
(*                easymesh err); *)
  (* Read the result *)
  let mesh = read (Array2.layout pslg.point) fname in
  Sys.remove (fname ^ ".d");
  Sys.remove (fname ^ ".n");
  Sys.remove (fname ^ ".e");
  Sys.remove (fname ^ ".s");
  mesh
