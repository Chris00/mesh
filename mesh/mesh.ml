(*pp camlp4o pa_macro.cmo *)
(* File: mesh.ml

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


open Bigarray
open Printf

type 'layout vec = (float, float64_elt, 'layout) Array1.t
type 'layout mat = (float, float64_elt, 'layout) Array2.t
type 'layout int_vec = (int, int_elt, 'layout) Array1.t
type 'layout int_mat = (int, int_elt, 'layout) Array2.t

class ['l] pslg (layout : 'l layout) =
object
  method point = Array2.create float64 layout 2 0
  method point_marker = Array1.create int layout 0
  method segment = Array2.create int layout 2 0
  method segment_marker = Array1.create int layout 0
  method hole = Array2.create float64 layout 2 0
  method region = Array2.create float64 layout 2 0
end


class type ['layout] t =
object
  inherit ['layout] pslg

  method triangle : 'layout int_mat
  method neighbor : 'layout int_mat
  method edge : 'layout int_mat
  method edge_marker : 'layout int_vec
end

class type ['layout] voronoi =
object
  method point : 'layout mat
  method edge  : 'layout int_mat
  method normal: 'layout mat
end



(** LaTeX commands *)

let norm dx dy =
  (* Watch out for overflow in computing sqrt(dx^2 + dy^2) *)
  let dx = abs_float dx and dy = abs_float dy in
  if dx = 0.0 then dy
  else if dy = 0.0 then dx
  else if dx >= dy then
    let q = dy /. dx in dx *. sqrt(1.0 +. q *. q)
  else
    let q = dx /. dy in dy *. sqrt(1.0 +. q *. q)

(*
let latex_begin fh width height xmin ymin =
  fprintf fh "\\begin{picture}(%.13g,%.13g)(%.13g,%.13g)\n"
    width height xmin ymin;
  fprintf fh "  %% \\meshline{x}{y}{angle}{length}\n";
  fprintf fh "  \\providecommand{\\meshline}[5]{%%
    \\put(#2,#3){\\rotatebox{#4}{\\rlap{\\smash{%%
      \\color{#1}%%
      \\vrule width #5\\unitlength height 0.1pt depth 0.1pt}}}}}\n";
  fprintf fh "  \\providecommand{\\meshpoint}[3]{%%
    \\put(#2,#3){\\makebox(0,0){\\footnotesize $\\bullet$}}}\n"

let latex_end fh =
  fprintf fh "\\end{picture}\n"
*)

(* PGF output *)
let latex_begin fh width height xmin ymin =
  fprintf fh "\\begin{pgfscope}\n";
  fprintf fh "  %% \\meshline{color}{x1}{y1}{x2}{y2}\n";
  (* We need to put the path in a scope otherwise one gets "TeX
     capacity exceeded". *)
  fprintf fh "  \\providecommand{\\meshline}[5]{%%
    \\begin{pgfscope}
      \\pgfsetcolor{#1}
      \\pgfpathmoveto{\\pgfpointxy{#2}{#3}}
      \\pgfpathlineto{\\pgfpointxy{#4}{#5}}
      \\pgfusepath{stroke}
    \\end{pgfscope}}\n";
  fprintf fh "  %% \\meshpoint{point number}{x}{y}\n";
  fprintf fh "  \\providecommand{\\meshpoint}[3]{}\n";
  fprintf fh "  %% \\meshtriangle{color}{x1}{y1}{x2}{y2}{x3}{y3}\n";
  fprintf fh "  \\providecommand{\\meshtriangle}[7]{%%
    \\begin{pgfscope}
      \\pgfsetcolor{#1}
      \\pgfpathmoveto{\\pgfpointxy{#2}{#3}}
      \\pgfpathlineto{\\pgfpointxy{#4}{#5}}
      \\pgfpathlineto{\\pgfpointxy{#6}{#7}}
      \\pgfusepath{fill}
    \\end{pgfscope}}\n"

let latex_end fh =
  fprintf fh "\\end{pgfscope}\n"


let degrees_per_radian = 45. /. atan 1.

(* More efficient than couples of floats *)
type point = { x : float; y : float }

let line fh color {x=x0; y=y0} {x=x1; y=y1} =
(*  let dx = x1 -. x0
  and dy = y1 -. y0 in
      fprintf fh "  \\meshline{%s}{%.12f}{%.12f}{%.12f}{%.12f}\n%!"
      color x0 y0 (degrees_per_radian *. atan2 dy dx) (norm dx dy)
*)
  fprintf fh "  \\meshline{%s}{%.12f}{%.12f}{%.12f}{%.12f}\n%!"
    color x0 y0 x1 y1

let point_xy fh i x y =
  fprintf fh "  \\meshpoint{%i}{%.12f}{%.13f}\n" i x y

let point fh i {x=x; y=y} = point_xy fh i x y

let triangle fh color {x=x1; y=y1} {x=x2; y=y2} {x=x3; y=y3} =
  fprintf fh "  \\meshtriangle{%s}{%.12f}{%.12f}{%.12f}{%.12f}{%.12f}{%.12f}\n"
    color x1 y1 x2 y2 x3 y3


(* Intersection of the curve et level [l] and the line passing through
   (x1,y1) and (x2,y2).  [z1 <> z2] assumed. *)
let intercept {x=x1; y=y1} z1 {x=x2; y=y2} z2 l =
  let d = z1 -. z2 and a = l -. z2 and b = z1 -. l in
  {x = (a *. x1 +. b *. x2) /. d;  y = (a *. y1 +. b *. y2) /. d }

let mid p q = {x = 0.5 *. (p.x +. q.x);  y = 0.5 *. (p.y +. q.y) }

let level_eq_default l1 l2 =
  abs_float(l1 -. l2) <= 1E-8 *. (abs_float l1 +. abs_float l2)

module M = Map.Make(struct
                      type t = int
                      let compare x y = compare (x:int) y
                    end)

(* Module to build a structure helping to determine when the segment
   joining 2 points are on the boundary. *)
module Edge =
struct
  let make() = ref M.empty

  let add_edge t i1 i2 =
    assert(i1 < i2);
    try
      let v = M.find i1 !t in
      v := i2 :: !v
    with Not_found ->
      t := M.add i1 (ref [i2]) !t

  (* Declare the segment joining the points of indexes [i1] and [i2]
     as being part of the boundary.   It is auusmed that [i1 <> i2]. *)
  let add t i1 i2 =
    if i1 < i2 then add_edge t i1 i2 else add_edge t i2 i1

  let on_boundary t i1 i2 =
    assert(i1 < i2);
    try
      let v = M.find i1 !t in List.mem i2 !v
    with Not_found -> false

  (* Tells whether the segment (if any) joining the points of indices
     [i1] and [i2] is on the boundary (according to the information in
     [t]).  It is assumed that [i1 <> i2]. *)
  let on t i1 i2 =
    if i1 < i2 then on_boundary t i1 i2 else on_boundary t i2 i1
end;;

(* Functions for fortran layout.
 ***********************************************************************)

module F =
struct
  type mesh = fortran_layout t;;
  type 'a vector = 'a vec               (* global vec *)
  type vec = fortran_layout vector;;    (* local vec *)
  type matrix = fortran_layout mat;;
  DEFINE NCOLS(a) = Array2.dim2 a;;
  DEFINE FST = 1;;
  DEFINE SND = 2;;
  DEFINE THIRD = 3;;
  DEFINE LASTCOL(a) = Array2.dim2 a;;
  DEFINE LASTEL(v) = Array1.dim v;;
  DEFINE GET(a,i,j) = a.{i,j};; (* the type of [a] must known at use point *)
  INCLUDE "meshFC.ml";;
end

module C =
struct
  type mesh = c_layout t;;
  type 'a vector = 'a vec               (* global vec *)
  type vec = c_layout vector;;
  DEFINE NCOLS(a) = Array2.dim1 a;;
  DEFINE FST = 0;;
  DEFINE SND = 1;;
  DEFINE THIRD = 2;;
  DEFINE LASTCOL(a) = Array2.dim1 a - 1;;
  DEFINE LASTEL(v) = Array1.dim v - 1;;
  DEFINE GET(a,i,j) = a.{j,i};;
  INCLUDE "meshFC.ml";;
end

let is_c_layout (mesh: _ pslg) =
  Array2.layout mesh#point = (Obj.magic c_layout : 'a Bigarray.layout)

module LaTeX =
struct

  let save (mesh: _ t) filename =
    if is_c_layout(mesh :> _ pslg)
    then C.latex (Obj.magic mesh) filename
    else F.latex (Obj.magic mesh) filename

  let level_curves ?boundary (mesh: 'a t) (z: 'a vec)
      ?level_eq levels filename =
    if is_c_layout(mesh :> _ pslg) then
      C.level_curves ?boundary (Obj.magic mesh) (Obj.magic z)
        ?level_eq levels filename
    else
      F.level_curves ?boundary (Obj.magic mesh) (Obj.magic z)
        ?level_eq levels filename
end

let scilab (mesh: 'a t) (z: 'a vec) filename =
  if is_c_layout(mesh :> _ pslg)
  then C.scilab (Obj.magic mesh) (Obj.magic z) filename
  else F.scilab (Obj.magic mesh) (Obj.magic z) filename

let matlab (mesh: 'a t) (z: 'a vec) filename =
  if is_c_layout(mesh :> _ pslg)
  then C.matlab (Obj.magic mesh) (Obj.magic z) filename
  else F.matlab (Obj.magic mesh) (Obj.magic z) filename


(* Local Variables: *)
(* compile-command: "make -k mesh.cmo" *)
(* End: *)
