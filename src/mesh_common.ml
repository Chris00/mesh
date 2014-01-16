(* File: mesh_common.ml

   Copyright (C) 2014

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

open Printf
open Bigarray

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

let layout (mesh: _ #pslg) = Array2.layout mesh#point

let is_c_layout (mesh: _ #pslg) =
  Mesh_utils.is_c_layout(Array2.layout mesh#point)

let pslg_to_c (m: _ #pslg) = (Obj.magic m : c_layout pslg)
let pslg_to_fortran (m: _ #pslg) = (Obj.magic m : fortran_layout pslg)

let mesh_to_c (m: _ #t) = (Obj.magic m : c_layout t)
let mesh_to_fortran (m: _ #t) = (Obj.magic m : fortran_layout t)

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
