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



(* LaTeX commands *)
let norm dx dy =
  (* Watch out for overflow in computing sqrt(dx^2 + dy^2) *)
  let dx = abs_float dx and dy = abs_float dy in
  if dx = 0.0 then dy
  else if dy = 0.0 then dx
  else if dx >= dy then
    let q = dy /. dx in dx *. sqrt(1.0 +. q *. q)
  else
    let q = dx /. dy in dy *. sqrt(1.0 +. q *. q)

let latex_header fh width height xmin ymin =
  fprintf fh "\\begin{picture}(%.13g,%.13g)(%.13g,%.13g)\n"
    width height xmin ymin;
  fprintf fh "  %% \\meshline{x}{y}{angle}{length}\n";
  fprintf fh "  \\providecommand{\\meshline}[5]{%%
    \\put(#2,#3){\\rotatebox{#4}{\\rlap{\\smash{%%
      \\color{#1}%%
      \\vrule width #5\\unitlength height 0.1pt depth 0.1pt}}}}}\n";
  fprintf fh "  \\providecommand{\\meshpoint}[3]{%%
    \\put(#2,#3){\\makebox(0,0){\\footnotesize $\\bullet$}}}\n"

let degrees_per_radian = 45. /. atan 1.

let line fh color (x0, y0) (x1, y1) =
  let dx = x1 -. x0
  and dy = y1 -. y0 in
  fprintf fh "  \\meshline{%s}{%.12f}{%.12f}{%.12f}{%.12f}\n%!"
    color x0 y0 (degrees_per_radian *. atan2 dy dx) (norm dx dy)

let point fh i x y =
  fprintf fh "  \\meshpoint{%i}{%.12f}{%.13f}\n" i x y


(* Intersection of the curve et level [l] and the line passing through
   (x1,y1) and (x2,y2).  [z1 <> z2] assumed. *)
let intercept x1 y1 z1 x2 y2 z2 l =
  let d = z1 -. z2 and a = l -. z2 and b = z1 -. l in
  ((a *. x1 +. b *. x2)/. d,  (a *. y1 +. b *. y2)/. d)

(* Functions for fortran layout.
 ***********************************************************************)

let bounding_box_fortran (mesh : fortran_layout t) =
  let xmin = ref infinity
  and xmax = ref neg_infinity
  and ymin = ref infinity
  and ymax = ref neg_infinity in
  for i = 1 to Array2.dim2(mesh.point) do
    let x = mesh.point.{1,i}
    and y = mesh.point.{2,i} in
    if x > !xmax then xmax := x;
    if x < !xmin then xmin := x;
    if y > !ymax then ymax := y;
    if y < !ymin then ymin := y;
  done;
  (!xmin, !xmax, !ymin, !ymax)

let latex_fortran (mesh : fortran_layout t) filename =
  let fh = open_out filename in
  let xmin, xmax, ymin, ymax = bounding_box_fortran mesh in
  latex_header fh (xmax -. xmin) (ymax -. ymin) xmin ymin;
  (* Write lines *)
  fprintf fh "  %% %i edges\n" ((Array2.dim2 mesh.triangle));
  for e = 1 to Array2.dim2(mesh.edge) do
    let i1 = mesh.edge.{1,e}
    and i2 = mesh.edge.{2,e} in
    let x1 = mesh.point.{1,i1}
    and y1 = mesh.point.{2,i1}
    and x2 = mesh.point.{1,i2}
    and y2 = mesh.point.{2,i2} in
    line fh "black" (x1, y1) (x2, y2)
  done;
  (* Write points *)
  fprintf fh "  %% %i points\n" ((Array2.dim2 mesh.point));
  for i = 1 to Array2.dim2(mesh.point) do
    point fh i mesh.point.{1,i} mesh.point.{2,i};
  done;
  (* Write trailer *)
  fprintf fh "\\end{picture}\n";
  close_out fh


let scilab_fortran (mesh : fortran_layout t) (z: fortran_layout vec) fname =
  let fname = Filename.chop_extension fname in
  let sci = fname ^ ".sci"
  and xf = fname ^ "_x.dat"
  and yf = fname ^ "_y.dat"
  and zf = fname ^ "_z.dat" in
  let fh = open_out sci in
  fprintf fh "// Run with exec('%s')
xf = fscanfMat('%s');
yf = fscanfMat('%s');
zf = fscanfMat('%s');
xbasc();
plot3d(xf, yf, zf)\n" sci xf yf zf;
  close_out fh;
  let save_mat fname coord =
    let fh = open_out fname in
    (* We traverse several times the triangles but Scilab will not
       have to transpose the matrices. *)
    for point = 1 to 3 do
      for t = 1 to Array2.dim2(mesh.triangle) do
        fprintf fh "%.13g " (coord mesh.triangle.{point,t})
      done;
      fprintf fh "\n";
    done;
    close_out fh in
  let pt = mesh.point in
  save_mat xf (fun i -> pt.{1,i});
  save_mat yf (fun i -> pt.{2,i});
  save_mat zf (fun i -> z.{i})


let level_curves_fortran ?(boundary=(fun _ -> "black"))
    (mesh : fortran_layout t) (z: fortran_layout vec) levels fname =
  let fh = open_out fname in
  let xmin, xmax, ymin, ymax = bounding_box_fortran mesh in
  latex_header fh (xmax -. xmin) (ymax -. ymin) xmin ymin;
  (* Draw the boundaries *)
  let edge = mesh.edge in
  let marker = mesh.edge_marker in
  for e = 1 to Array2.dim2(edge) do
    let m = marker.{e} in
    if m <> 0 then begin
      let i1 = mesh.edge.{1,e}
      and i2 = mesh.edge.{2,e} in
      let x1 = mesh.point.{1,i1}
      and y1 = mesh.point.{2,i1}
      and x2 = mesh.point.{1,i2}
      and y2 = mesh.point.{2,i2} in
      line fh (boundary m) (x1, y1) (x2, y2)
    end
  done;
  let tr = mesh.triangle in
  let pt = mesh.point in
  for t = 1 to Array2.dim2(tr) do
    let i1 = tr.{1,t}
    and i2 = tr.{2,t}
    and i3 = tr.{3,t} in
    let x1 = pt.{1,i1}
    and y1 = pt.{2,i1}
    and z1 = z.{i1} in
    let x2 = pt.{1,i2}
    and y2 = pt.{2,i2}
    and z2 = z.{i2} in
    let x3 = pt.{1,i3}
    and y3 = pt.{2,i3}
    and z3 = z.{i3} in
    List.iter
      (fun l ->
         (* Draw the level curve [l] on the triangle [t]. *)
         if l < z1 then
           if l < z2 then
             if l > z3 then
               line fh "black" (intercept x1 y1 z1 x3 y3 z3 l)
                 (intercept x2 y2 z2 x3 y3 z3 l)
             else if l = z3 then
               point fh i3 x3 y3
             else ()
           else if l = z2 then
             if l >= z3 then (* z3 <= l = z2 < z1 *)
               line fh "black" (x2,y2) (intercept x1 y1 z1 x3 y3 z3 l)
             else
               point fh i2 x2 y2
           else (* l > z2 *)
             line fh "black" (intercept x1 y1 z1 x2 y2 z2 l)
               (if l < z3 then      intercept x2 y2 z2 x3 y3 z3 l
                else if l > z3 then intercept x1 y1 z1 x3 y3 z3 l
                else (* l = z3 *)   (x3,y3))

         else if l > z1 then
           (* Symmetric of [l < z1] with all inequalities reversed *)
           if l > z2 then
             if l < z3 then
               line fh "black" (intercept x1 y1 z1 x3 y3 z3 l)
                 (intercept x2 y2 z2 x3 y3 z3 l)
             else if l = z3 then
               point fh i3 x3 y3
             else ()
           else if l = z2 then
             if l <= z3 then (* z3 >= l = z2 > z1 *)
               line fh "black" (x2,y2) (intercept x1 y1 z1 x3 y3 z3 l)
             else
               point fh i2 x2 y2
           else (* l < z2 *)
             line fh "black" (intercept x1 y1 z1 x2 y2 z2 l)
               (if l > z3 then      intercept x2 y2 z2 x3 y3 z3 l
                else if l < z3 then intercept x1 y1 z1 x3 y3 z3 l
                else (* l = z3 *)   (x3,y3))

         else (* l = z1 *)
           if (z2 <= l && l < z3) || (z3 <= l && l < z2) then
             line fh "black" (x1,y1) (intercept x2 y2 z2 x3 y3 z3 l)
           else if l = z2 && l = z3 then (
             (* Full triangle ! *)
             line fh "black" (x1,y1) (x2,y2);
             line fh "black" (x2,y2) (x3,y3);
             line fh "black" (x3,y3) (x1,y1);
           )
           else ()
      ) levels
  done;
  (* Write trailer *)
  fprintf fh "\\end{picture}\n";
  close_out fh


(* Functions for c layout.
 ***********************************************************************)

let bounding_box_c (mesh : c_layout t) =
  let xmin = ref infinity
  and xmax = ref neg_infinity
  and ymin = ref infinity
  and ymax = ref neg_infinity in
  for i = 0 to Array2.dim1(mesh.point) - 1 do
    let x = mesh.point.{i,0}
    and y = mesh.point.{i,1} in
    if x > !xmax then xmax := x;
    if x < !xmin then xmin := x;
    if y > !ymax then ymax := y;
    if y < !ymin then ymin := y;
  done;
  (!xmin, !xmax, !ymin, !ymax)

let latex_c (mesh : c_layout t) filename =
  let fh = open_out filename in
  let xmin, xmax, ymin, ymax = bounding_box_c mesh in
  latex_header fh (xmax -. xmin) (ymax -. ymin) xmin ymin;
  (* Write lines *)
  fprintf fh "  %% %i edges\n" ((Array2.dim1 mesh.triangle));
  for e = 0 to Array2.dim1(mesh.edge) - 1 do
    let i1 = mesh.edge.{e,0}
    and i2 = mesh.edge.{e,1} in
    let x1 = mesh.point.{i1,0}
    and y1 = mesh.point.{i1,1}
    and x2 = mesh.point.{i2,0}
    and y2 = mesh.point.{i2,1} in
    line fh "black" (x1, y1) (x2, y2)
  done;
  (* Write points *)
  fprintf fh "  %% %i points\n" ((Array2.dim1 mesh.point));
  for i = 0 to Array2.dim1(mesh.point) - 1 do
    point fh i mesh.point.{i,0} mesh.point.{i,1};
  done;
  (* Write trailer *)
  fprintf fh "\\end{picture}\n";
  close_out fh


let scilab_c (mesh : c_layout t) (z: c_layout vec) fname =
  let fname = Filename.chop_extension fname in
  let sci = fname ^ ".sci"
  and xf = fname ^ "_x.dat"
  and yf = fname ^ "_y.dat"
  and zf = fname ^ "_z.dat" in
  let fh = open_out sci in
  fprintf fh "// Run with exec('%s')
xf = fscanfMat('%s');
yf = fscanfMat('%s');
zf = fscanfMat('%s');
xbasc();
plot3d(xf, yf, zf)\n" sci xf yf zf;
  close_out fh;
  let save_mat fname coord =
    let fh = open_out fname in
    (* We traverse several times the triangles but Scilab will not
       have to transpose the matrices. *)
    for point = 0 to 2 do
      for t = 0 to Array2.dim1(mesh.triangle) - 1 do
        fprintf fh "%.13g " (coord mesh.triangle.{t,point})
      done;
      fprintf fh "\n";
    done;
    close_out fh in
  let pt = mesh.point in
  save_mat xf (fun i -> pt.{i,0});
  save_mat yf (fun i -> pt.{i,1});
  save_mat zf (fun i -> z.{i})


let level_curves_c ?(boundary=(fun _ -> "black"))
    (mesh : c_layout t) (z: c_layout vec) levels fname =
  let fh = open_out fname in
  let xmin, xmax, ymin, ymax = bounding_box_c mesh in
  latex_header fh (xmax -. xmin) (ymax -. ymin) xmin ymin;
  (* Draw the boundaries *)
  let edge = mesh.edge in
  let marker = mesh.edge_marker in
  for e = 0 to Array2.dim1(edge) - 1 do
    let m = marker.{e} in
    if m <> 0 then begin
      let i1 = mesh.edge.{e,0}
      and i2 = mesh.edge.{e,1} in
      let x1 = mesh.point.{i1,0}
      and y1 = mesh.point.{i1,1}
      and x2 = mesh.point.{i2,0}
      and y2 = mesh.point.{i2,1} in
      line fh (boundary m) (x1, y1) (x2, y2)
    end
  done;
  let tr = mesh.triangle in
  let pt = mesh.point in
  for t = 0 to Array2.dim1(tr) - 1 do
    let i1 = tr.{t,0}
    and i2 = tr.{t,1}
    and i3 = tr.{t,2} in
    let x1 = pt.{i1,0}
    and y1 = pt.{i1,1}
    and z1 = z.{i1} in
    let x2 = pt.{i2,0}
    and y2 = pt.{i2,1}
    and z2 = z.{i2} in
    let x3 = pt.{i3,0}
    and y3 = pt.{i3,1}
    and z3 = z.{i3} in
    List.iter
      (fun l ->
         (* Draw the level curve [l] on the triangle [t]. *)
         if l < z1 then
           if l < z2 then
             if l > z3 then
               line fh "black" (intercept x1 y1 z1 x3 y3 z3 l)
                 (intercept x2 y2 z2 x3 y3 z3 l)
             else if l = z3 then
               point fh i3 x3 y3
             else ()
           else if l = z2 then
             if l >= z3 then (* z3 <= l = z2 < z1 *)
               line fh "black" (x2,y2) (intercept x1 y1 z1 x3 y3 z3 l)
             else
               point fh i2 x2 y2
           else (* l > z2 *)
             line fh "black" (intercept x1 y1 z1 x2 y2 z2 l)
               (if l < z3 then      intercept x2 y2 z2 x3 y3 z3 l
                else if l > z3 then intercept x1 y1 z1 x3 y3 z3 l
                else (* l = z3 *)   (x3,y3))

         else if l > z1 then
           (* Symmetric of [l < z1] with all inequalities reversed *)
           if l > z2 then
             if l < z3 then
               line fh "black" (intercept x1 y1 z1 x3 y3 z3 l)
                 (intercept x2 y2 z2 x3 y3 z3 l)
             else if l = z3 then
               point fh i3 x3 y3
             else ()
           else if l = z2 then
             if l <= z3 then (* z3 >= l = z2 > z1 *)
               line fh "black" (x2,y2) (intercept x1 y1 z1 x3 y3 z3 l)
             else
               point fh i2 x2 y2
           else (* l < z2 *)
             line fh "black" (intercept x1 y1 z1 x2 y2 z2 l)
               (if l > z3 then      intercept x2 y2 z2 x3 y3 z3 l
                else if l < z3 then intercept x1 y1 z1 x3 y3 z3 l
                else (* l = z3 *)   (x3,y3))

         else (* l = z1 *)
           if (z2 <= l && l < z3) || (z3 <= l && l < z2) then
             line fh "black" (x1,y1) (intercept x2 y2 z2 x3 y3 z3 l)
           else if l = z2 && l = z3 then (
             (* Full triangle ! *)
             line fh "black" (x1,y1) (x2,y2);
             line fh "black" (x2,y2) (x3,y3);
             line fh "black" (x3,y3) (x1,y1);
           )
           else ()
      ) levels
  done;
  (* Write trailer *)
  fprintf fh "\\end{picture}\n";
  close_out fh



let is_c_layout mesh =
  Array2.layout mesh.point = (Obj.magic c_layout : 'a Bigarray.layout)

let latex mesh filename =
  if is_c_layout mesh then latex_c (Obj.magic mesh) filename
  else latex_fortran (Obj.magic mesh) filename

let scilab (mesh: 'a t) (z: 'a vec) filename =
  if is_c_layout mesh then scilab_c (Obj.magic mesh) (Obj.magic z) filename
  else scilab_fortran (Obj.magic mesh) (Obj.magic z) filename

let level_curves ?boundary (mesh: 'a t) (z: 'a vec) levels filename =
  if is_c_layout mesh then
    level_curves_c ?boundary (Obj.magic mesh) (Obj.magic z) levels filename
  else
    level_curves_fortran ?boundary (Obj.magic mesh) (Obj.magic z)
      levels filename
