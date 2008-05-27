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

type 'layout t = {
  point : (float, float64_elt, 'layout) Array2.t;
  point_marker : (int, int_elt, 'layout) Array1.t;
  triangle : (int, int_elt, 'layout) Array2.t;
  neighbor : (int, int_elt, 'layout) Array2.t;
  edge : (int, int_elt, 'layout) Array2.t;
  edge_marker : (int, int_elt, 'layout) Array1.t;

  segment : (int, int_elt, 'layout) Array2.t;
  segment_marker : (int, int_elt, 'layout) Array1.t;
  hole : (float, float64_elt, 'layout) Array2.t;
  region : (float, float64_elt, 'layout) Array2.t;
}


let empty layout =
  let empty_array = Array2.create float64 layout 0 0
  and empty_int_array = Array2.create int layout 0 0
  and empty_int_vec =  Array1.create int layout 0 in
  {
    point = empty_array;
    point_marker = empty_int_vec;
    triangle = empty_int_array;
    neighbor = empty_int_array;
    edge = empty_int_array;
    edge_marker = empty_int_vec;

    segment = empty_int_array;
    segment_marker = empty_int_vec;
    hole = empty_array;
    region = empty_array;
  }


type 'layout voronoi = {
  vor_point : (float, float64_elt, 'layout) Array2.t;
  vor_edge : (int, int_elt, 'layout) Array2.t;
  vor_normal : (float, float64_elt, 'layout) Array2.t;
}



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

let line fh color (x0, y0) (x1, y1) =
(*  let dx = x1 -. x0
  and dy = y1 -. y0 in
      fprintf fh "  \\meshline{%s}{%.12f}{%.12f}{%.12f}{%.12f}\n%!"
      color x0 y0 (degrees_per_radian *. atan2 dy dx) (norm dx dy)
*)
  fprintf fh "  \\meshline{%s}{%.12f}{%.12f}{%.12f}{%.12f}\n%!"
    color x0 y0 x1 y1


let point fh i x y =
  fprintf fh "  \\meshpoint{%i}{%.12f}{%.13f}\n" i x y

let triangle fh color x1 y1 x2 y2 x3 y3 =
  fprintf fh "  \\meshtriangle{%s}{%.12f}{%.12f}{%.12f}{%.12f}{%.12f}{%.12f}\n"
    color x1 y1 x2 y2 x3 y3


(* Intersection of the curve et level [l] and the line passing through
   (x1,y1) and (x2,y2).  [z1 <> z2] assumed. *)
let intercept x1 y1 z1 x2 y2 z2 l =
  let d = z1 -. z2 and a = l -. z2 and b = z1 -. l in
  ((a *. x1 +. b *. x2)/. d,  (a *. y1 +. b *. y2)/. d)

(* Functions for fortran layout.
 ***********************************************************************)

module F =
struct
  type mesh = fortran_layout t;;
  type 'a vector = 'a vec               (* global vec *)
  type vec = fortran_layout vector;;    (* local vec *)
  DEFINE NCOLS(a) = Array2.dim2 a;;
  DEFINE FST = 1;;
  DEFINE SND = 2;;
  DEFINE THIRD = 3;;
  DEFINE LASTCOL(a) = Array2.dim2 a;;
  DEFINE GET(a,i,j) = a.{i,j};;
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
  DEFINE GET(a,i,j) = a.{j,i};;
  INCLUDE "meshFC.ml";;
end

let is_c_layout mesh =
  Array2.layout mesh.point = (Obj.magic c_layout : 'a Bigarray.layout)

let latex mesh filename =
  if is_c_layout mesh then C.latex (Obj.magic mesh) filename
  else F.latex (Obj.magic mesh) filename

let scilab (mesh: 'a t) (z: 'a vec) filename =
  if is_c_layout mesh then C.scilab (Obj.magic mesh) (Obj.magic z) filename
  else F.scilab (Obj.magic mesh) (Obj.magic z) filename

let level_curves ?boundary (mesh: 'a t) (z: 'a vec) levels filename =
  if is_c_layout mesh then
    C.level_curves ?boundary (Obj.magic mesh) (Obj.magic z) levels filename
  else
    F.level_curves ?boundary (Obj.magic mesh) (Obj.magic z) levels filename
