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

let max2 a b = if (a:int) > b then a else b
let max4 a b c d = max2 (max2 a b) (max2 c d)

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

class ['l] alias (mesh: 'l #t) =
object
  method point = mesh#point
  method point_marker = mesh#point_marker
  method segment = mesh#segment
  method segment_marker = mesh#segment_marker
  method hole = mesh#hole
  method region = mesh#region
  method triangle = mesh#triangle
  method neighbor = mesh#neighbor
  method edge = mesh#edge
  method edge_marker = mesh#edge_marker
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
  fprintf fh "  %% \\meshline{R,G,B}{x1}{y1}{x2}{y2}\n";
  (* We need to put the path in a scope otherwise one gets "TeX
     capacity exceeded". *)
  fprintf fh "  \\providecommand{\\meshline}[5]{%%
    \\begin{pgfscope}
      \\definecolor{ocamlmesh}{RGB}{#1}
      \\pgfsetcolor{ocamlmesh}
      \\pgfpathmoveto{\\pgfpointxy{#2}{#3}}
      \\pgfpathlineto{\\pgfpointxy{#4}{#5}}
      \\pgfusepath{stroke}
    \\end{pgfscope}}\n";
  fprintf fh "  %% \\meshpoint{point number}{x}{y}\n";
  fprintf fh "  \\providecommand{\\meshpoint}[3]{}\n";
  fprintf fh "  %% \\meshtriangle{R,G,B}{x1}{y1}{x2}{y2}{x3}{y3}\n";
  fprintf fh "  \\providecommand{\\meshtriangle}[7]{%%
    \\begin{pgfscope}
      \\definecolor{ocamlmesh}{RGB}{#1}
      \\pgfsetcolor{ocamlmesh}
      \\pgfpathmoveto{\\pgfpointxy{#2}{#3}}
      \\pgfpathlineto{\\pgfpointxy{#4}{#5}}
      \\pgfpathlineto{\\pgfpointxy{#6}{#7}}
      \\pgfusepath{fill}
    \\end{pgfscope}}\n";
  fprintf fh "  %% \\meshfilltriangle{R,G,B}{x1}{y1}{x2}{y2}{x3}{y3}\n";
  fprintf fh "  \\providecommand{\\meshfilltriangle}[7]{%%
    \\begin{pgfscope}
      \\definecolor{ocamlmesh}{RGB}{#1}
      \\pgfsetcolor{ocamlmesh}
      \\pgfpathmoveto{\\pgfpointxy{#2}{#3}}
      \\pgfpathlineto{\\pgfpointxy{#4}{#5}}
      \\pgfpathlineto{\\pgfpointxy{#6}{#7}}
      \\pgfusepath{fill}
    \\end{pgfscope}}\n";
  fprintf fh "  %% \\meshfillquadrilateral{R,G,B}{x1}{y1}{x2}{y2}{x3}{y3}\
    {x4}{y4}\n";
  fprintf fh "  \\providecommand{\\meshfillquadrilateral}[9]{%%
    \\begin{pgfscope}
      \\definecolor{ocamlmesh}{RGB}{#1}
      \\pgfsetcolor{ocamlmesh}
      \\pgfpathmoveto{\\pgfpointxy{#2}{#3}}
      \\pgfpathlineto{\\pgfpointxy{#4}{#5}}
      \\pgfpathlineto{\\pgfpointxy{#6}{#7}}
      \\pgfpathlineto{\\pgfpointxy{#8}{#9}}
      \\pgfusepath{fill}
    \\end{pgfscope}}\n"

let latex_end fh =
  fprintf fh "\\end{pgfscope}\n"


let degrees_per_radian = 45. /. atan 1.

(* More efficient than couples of floats *)
type point = { x : float; y : float }

let black = 0x000000

let color_to_string c =
  let b = c land 0xFF in
  let g = (c lsr 8) land 0xFF in
  let r = (c lsr 16) land 0xFF in
  sprintf "%i,%i,%i" r g b

let line fh color {x=x0; y=y0} {x=x1; y=y1} =
(*  let dx = x1 -. x0
  and dy = y1 -. y0 in
      fprintf fh "  \\meshline{%s}{%.12f}{%.12f}{%.12f}{%.12f}\n%!"
      color x0 y0 (degrees_per_radian *. atan2 dy dx) (norm dx dy)
*)
  fprintf fh "  \\meshline{%s}{%.12f}{%.12f}{%.12f}{%.12f}\n%!"
    (color_to_string color) x0 y0 x1 y1

let point_xy fh i x y =
  fprintf fh "  \\meshpoint{%i}{%.12f}{%.13f}\n" i x y

let point fh i {x=x; y=y} = point_xy fh i x y

let triangle fh color {x=x1; y=y1} {x=x2; y=y2} {x=x3; y=y3} =
  fprintf fh "  \\meshtriangle{%s}{%.12f}{%.12f}{%.12f}{%.12f}{%.12f}{%.12f}\n"
    (color_to_string color) x1 y1 x2 y2 x3 y3

let fill_triangle fh color {x=x1; y=y1} {x=x2; y=y2} {x=x3; y=y3} =
  fprintf fh "  \\meshfilltriangle{%s}{%.12f}{%.12f}{%.12f}{%.12f}\
    {%.12f}{%.12f}\n"
    (color_to_string color) x1 y1 x2 y2 x3 y3

let fill_quadrilateral fh color {x=x1; y=y1} {x=x2; y=y2} {x=x3; y=y3}
    {x=x4; y=y4} =
  fprintf fh "  \\meshfillquadrilateral{%s}{%.12f}{%.12f}{%.12f}{%.12f}\
    {%.12f}{%.12f}{%.12f}{%.12f}\n"
    (color_to_string color) x1 y1 x2 y2 x3 y3 x4 y4



(* Functions for fortran layout.
 ***********************************************************************)

module F =
struct
  type mesh = fortran_layout t;;
  type 'a vector = 'a vec               (* global vec *)
  type vec = fortran_layout vector;;  (* local vec *)
  type matrix = fortran_layout mat;;
  type 'a int_vector = 'a int_vec
  type int_vec = fortran_layout int_vector
  let layout = fortran_layout;;
  DEFINE NCOLS(a) = Array2.dim2 a;;
  DEFINE NROWS(a) = Array2.dim1 a;;
  DEFINE FST = 1;;
  DEFINE SND = 2;;
  DEFINE THIRD = 3;;
  DEFINE TO_FORTRAN(i) = i;;
  DEFINE LASTCOL(a) = Array2.dim2 a;;
  DEFINE LASTROW(a) = Array2.dim1 a;;
  DEFINE LASTEL(v) = Array1.dim v;;
  DEFINE GET(a,i,j) = a.{i,j};; (* the type of [a] must known at use point *)
  DEFINE ARRAY2(kind, n, m) = Array2.create kind fortran_layout n m
  INCLUDE "meshFC.ml";;
end

module C =
struct
  type mesh = c_layout t;;
  type 'a vector = 'a vec               (* global vec *)
  type vec = c_layout vector;;
  type matrix = c_layout mat;;
  type 'a int_vector = 'a int_vec
  type int_vec = c_layout int_vector
  let layout = c_layout;;
  DEFINE NCOLS(a) = Array2.dim1 a;;
  DEFINE NROWS(a) = Array2.dim2 a;;
  DEFINE FST = 0;;
  DEFINE SND = 1;;
  DEFINE THIRD = 2;;
  DEFINE TO_FORTRAN(i) = i + 1;;
  DEFINE LASTCOL(a) = Array2.dim1 a - 1;;
  DEFINE LASTROW(a) = Array2.dim2 a - 1;;
  DEFINE LASTEL(v) = Array1.dim v - 1;;
  DEFINE GET(a,i,j) = a.{j,i};;
  DEFINE ARRAY2(kind, n, m) = Array2.create kind c_layout m n
  INCLUDE "meshFC.ml";;
end

let is_c_layout (mesh: _ #pslg) =
  Array2.layout mesh#point = (Obj.magic c_layout : 'a Bigarray.layout)

let band_height_P1 ?filter mesh =
  if is_c_layout mesh then
    C.band_height_P1 filter (Obj.magic(mesh: _ #t) : c_layout t)
  else
    F.band_height_P1 filter (Obj.magic(mesh: _ #t) : fortran_layout t)

let cuthill_mckee ?(rev=true) ?(perm: 'l int_vec option) (mesh: 'l #t) =
  if is_c_layout mesh then
    let m = C.cuthill_mckee ~rev (Obj.magic perm : c_layout int_vec option)
      (Obj.magic mesh : c_layout #t) in
    (Obj.magic (m: c_layout t) : 'l t)
  else
    let m = F.cuthill_mckee ~rev
      (Obj.magic perm : fortran_layout int_vec option)
      (Obj.magic mesh : fortran_layout #t) in
    (Obj.magic (m: fortran_layout t) : 'l t)

let permute ?(inv=false) (perm: 'l int_vec) (mesh: 'l #t) =
  if is_c_layout mesh then
    let m = C.permute inv (Obj.magic perm : c_layout int_vec)
      (Obj.magic mesh :  c_layout #t) in
    (Obj.magic (m: c_layout t) : 'l t)
  else
    let m = F.permute inv (Obj.magic perm : fortran_layout int_vec)
      (Obj.magic mesh : fortran_layout #t) in
    (Obj.magic (m: fortran_layout t) : 'l t)


module LaTeX =
struct
  type color = int

  let save ?edge (mesh: _ #t) filename =
    if is_c_layout(mesh :> _ pslg)
    then C.latex ?edge (Obj.magic mesh) filename
    else F.latex ?edge (Obj.magic mesh) filename

  let level_curves ?boundary (mesh: 'a #t) (z: 'a vec)
      ?level_eq levels filename =
    if is_c_layout(mesh :> _ pslg) then
      C.level_curves ?boundary (Obj.magic mesh) (Obj.magic z)
        ?level_eq levels filename
    else
      F.level_curves ?boundary (Obj.magic mesh) (Obj.magic z)
        ?level_eq levels filename

  let super_level ?boundary (mesh: 'a #t) (z: 'a vec) level color filename =
    if is_c_layout mesh then
      C.super_level ?boundary (Obj.magic mesh) (Obj.magic z)
        level color filename
    else
      F.super_level ?boundary (Obj.magic mesh) (Obj.magic z)
        level color filename

  let sub_level ?boundary (mesh: 'a #t) (z: 'a vec) level color filename =
    if is_c_layout mesh then
      C.sub_level ?boundary (Obj.magic mesh) (Obj.magic z)
        level color filename
    else
      F.sub_level ?boundary (Obj.magic mesh) (Obj.magic z)
        level color filename
end

let scilab (mesh: 'a #t) (z: 'a vec) filename =
  if is_c_layout mesh
  then C.scilab (Obj.magic mesh) (Obj.magic z) filename
  else F.scilab (Obj.magic mesh) (Obj.magic z) filename

let matlab (mesh: 'a #t) (z: 'a vec) filename =
  if is_c_layout mesh
  then C.matlab (Obj.magic mesh) (Obj.magic z) filename
  else F.matlab (Obj.magic mesh) (Obj.magic z) filename

let mathematica (mesh: 'a #t) (z: 'a vec) filename =
  if is_c_layout mesh
  then C.mathematica (Obj.magic mesh: _ t) (Obj.magic z: _ vec) filename
  else F.mathematica (Obj.magic mesh: _ t) (Obj.magic z: _ vec) filename


(* Local Variables: *)
(* compile-command: "make -k mesh.cmo" *)
(* End: *)
