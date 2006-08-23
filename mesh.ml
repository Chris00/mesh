open Bigarray

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



open Printf

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
  fprintf fh "  \\providecommand{\\meshline}[4]{%%
    \\put(#1,#2){\\rotatebox{#3}{\\rlap{\\smash{%%
      \\vrule width #4\\unitlength height 0.1pt depth 0.1pt}}}}}\n";
  fprintf fh "  \\providecommand{\\meshpoint}[3]{%%
    \\put(#2,#3){\\makebox(0,0){$\\bullet$}}}\n"

let degrees_per_radian = 45. /. atan 1.

let line fh x0 y0 x1 y1 =
  let dx = x1 -. x0
  and dy = y1 -. y0 in
  fprintf fh "  \\meshline{%.12f}{%.12f}{%.12f}{%.12f}\n%!"
    x0 y0 (degrees_per_radian *. atan2 dy dx) (norm dx dy)

let point fh i x y =
  fprintf fh "  \\meshpoint{%i}{%.12f}{%.13f}\n" i x y


let latex_c (mesh : c_layout t) filename =
  let fh = open_out filename in
  let triangle t =
    let i0 = mesh.triangle.{t,0}
    and i1 = mesh.triangle.{t,1}
    and i2 = mesh.triangle.{t,2} in
    let x0 = mesh.point.{i0,0}
    and y0 = mesh.point.{i0,1} in
    let x1 = mesh.point.{i1,0}
    and y1 = mesh.point.{i1,1} in
    let x2 = mesh.point.{i2,0}
    and y2 = mesh.point.{i2,1} in
    line fh x0 y0 x1 y1;
    line fh x1 y1 x2 y2;
    line fh x2 y2 x0 y0 in
  (* Write header *)
  let xmin = ref infinity
  and xmax = ref neg_infinity
  and ymin = ref infinity
  and ymax = ref neg_infinity in
  for i = 0 to Array2.dim1 mesh.point - 1 do
    let x = mesh.point.{i,0}
    and y = mesh.point.{i,1} in
    if x > !xmax then xmax := x;
    if x < !xmin then xmin := x;
    if y > !ymax then ymax := y;
    if y < !ymin then ymin := y;
  done;
  latex_header fh (!xmax -. !xmin) (!ymax -. !ymin) !xmin !ymin;
  (* Write lines *)
  fprintf fh "  %% %i triangles\n" (Array2.dim1 mesh.triangle);
  for t = 0 to Array2.dim1 mesh.triangle - 1 do triangle t done;
  (* Write points *)
  fprintf fh "  %% %i points\n" (Array2.dim1 mesh.point);
  for i = 0 to Array2.dim1 mesh.point - 1 do
    point fh i mesh.point.{i,0} mesh.point.{i,1};
  done;
  (* Write trailer *)
  fprintf fh "\\end{picture}\n";
  close_out fh


let latex_fortran (mesh : fortran_layout t) filename =
  let fh = open_out filename in
  let triangle t =
    let i0 = mesh.triangle.{1,t}
    and i1 = mesh.triangle.{2,t}
    and i2 = mesh.triangle.{3,t} in
    let x0 = mesh.point.{1,i0}
    and y0 = mesh.point.{2,i0} in
    let x1 = mesh.point.{1,i1}
    and y1 = mesh.point.{2,i1} in
    let x2 = mesh.point.{1,i2}
    and y2 = mesh.point.{2,i2} in
    line fh x0 y0 x1 y1;
    line fh x1 y1 x2 y2;
    line fh x2 y2 x0 y0 in
  (* Write header *)
  let xmin = ref infinity
  and xmax = ref neg_infinity
  and ymin = ref infinity
  and ymax = ref neg_infinity in
  for i = 1 to Array2.dim2 mesh.point do
    let x = mesh.point.{1,i}
    and y = mesh.point.{2,i} in
    if x > !xmax then xmax := x;
    if x < !xmin then xmin := x;
    if y > !ymax then ymax := y;
    if y < !ymin then ymin := y;
  done;
  latex_header fh (!xmax -. !xmin) (!ymax -. !ymin) !xmin !ymin;
  (* Write lines *)
  fprintf fh "  %% %i triangles\n" (Array2.dim2 mesh.triangle);
  for t = 1 to Array2.dim2 mesh.triangle do triangle t done;
  (* Write points *)
  fprintf fh "  %% %i points\n" (Array2.dim2 mesh.point);
  for i = 1 to Array2.dim2 mesh.point do
    point fh i mesh.point.{1,i} mesh.point.{2,i};
  done;
  (* Write trailer *)
  fprintf fh "\\end{picture}\n";
  close_out fh


let is_c_layout mesh =
  Array2.layout mesh.point = (Obj.magic c_layout : 'a Bigarray.layout)

let latex mesh filename =
  if is_c_layout mesh then latex_c (Obj.magic mesh) filename
  else latex_fortran (Obj.magic mesh) filename
