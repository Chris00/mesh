(* File: laplacian.ml

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

open Bigarray
open Mesh
open Lacaml.Impl.D

type mesh = fortran_layout Mesh.t
type vec = (float, float64_elt, fortran_layout) Array1.t

let max2 a b = if (a:int) > b then a else b
let max4 a b c d = max2 (max2 a b) (max2 c d)

(* Determine the number of superdiagonals + 1 main diagonal *)
let band_height (mesh: mesh) =
  let tr = mesh#triangle in
  let kd = ref 0 in
  for i = 1 to Array2.dim2 tr do
    let i1 = tr.{1,i}
    and i2 = tr.{2,i}
    and i3 = tr.{3,i} in
    kd := max4 !kd (abs(i1 - i2)) (abs(i2 -i3)) (abs(i3 - i1))
  done;
  !kd + 1

(* [sort3 a b c] sort [a], [b], [c] from the larger to the smaller *)
let sort3 (a:int) b c =
  if a > b then
    if a <= c then (c, a, b)
    else if b > c then (a, b, c) else (a, c, b)
  else (* a <= b *)
    if b <= c then (c, b, a)
    else if a > c then (b, a, c) else (b, c, a)


let inner_band_matrix (mesh: mesh) =
  let band = band_height mesh in
  let tr = mesh#triangle in
  let pt = mesh#point in
  let nnodes = Mat.dim2 pt in
  (* Inner product matrix, INCLUDING boundary points: *)
  let im = Mat.make0 band nnodes in
  let tr_area = Vec.create (Array2.dim2 tr) in
  for t = 1 to Array2.dim2 tr do
    let i1, i2, i3 = sort3 tr.{1,t} tr.{2,t} tr.{3,t} in (* i1 >= i2 >= i3 *)
    let x1 = pt.{1,i1}  and y1 = pt.{2,i1} in
    let x2 = pt.{1,i2}  and y2 = pt.{2,i2} in
    let x3 = pt.{1,i3}  and y3 = pt.{2,i3} in
    let x32 = x3 -. x2  and y23 = y2 -. y3 in
    let x13 = x1 -. x3  and y31 = y3 -. y1 in
    let x21 = x2 -. x1  and y12 = y1 -. y2 in
    let abs_det = abs_float(x21 *. y31 -. x13 *. y12) in
    tr_area.{t} <- 0.5 *. abs_det;
    let a = 0.5 /. abs_det in
    (* diagonal *)
    im.{band, i1} <- im.{band, i1} +. (y23 *. y23 +. x32 *. x32) *. a;
    im.{band, i2} <- im.{band, i2} +. (y31 *. y31 +. x13 *. x13) *. a;
    im.{band, i3} <- im.{band, i3} +. (y12 *. y12 +. x21 *. x21) *. a;
    (* above diag: k <= l => A(k,l) = im.{band + k - l, l} *)
    let j = band + i2 - i1 in
    im.{j, i1} <- im.{j, i1} +. (y23 *. y31 +. x32 *. x13) *. a;
    let j = band + i3 - i1 in
    im.{j, i1} <- im.{j, i1} +. (y23 *. y12 +. x32 *. x21) *. a;
    let j = band + i3 - i2 in
    im.{j, i2} <- im.{j, i2} +. (y31 *. y12 +. x13 *. x21) *. a;
  done;
  im, tr_area

type t = {
  mesh: mesh;                   (* original mesh *)
  area : vec;                   (* area of each triangle *)
  inner: mat;                   (* inner prodict matrix *)
  lap : mat;                    (* FEM band matrix of laplacian with BC *)
  bc_rhs : vec;                 (* Dirichlet boundary correction *)
  bc_dirichlet : int list;      (* Boundary Dirichlet nodes *)
}

let make ?(bc=(fun _ _ _ -> None)) (mesh: mesh) =
  let inner, area = inner_band_matrix mesh in
  (* Each boundary equation is changed and the contribution of the
     boundary nodes is kept in [bc_rhs].  If I denotes the initial
     matrix and I' the matrix after transformation, we have that
     Iv = b <=> I'v = b + bc_rhs *)
  let band = band_height mesh in
  let pt = mesh#point in
  let nnodes = Mat.dim2 pt in
  let lap = lacpy inner in
  let bc_rhs = Vec.make0 nnodes in
  let bc_dirichlet = ref [] in          (* RHS must be modif. for B.C. *)
  for i = 1 to nnodes do
    let marker = mesh#point_marker.{i} in
    if marker <> 0 then
      match bc marker pt.{1,i} pt.{2,i} with
      | None -> ()                      (* Neumann boundary cond *)
      | Some v ->                       (* sol = v at this point *)
          for k = 1 + max2 0 (band - i) to band - 1 do
            let j = i - band + k in     (* above diag *)
            (* This may add a contribution to bc_rhs.{j} for previously
               modified lines j but this is no problem because
               lap.{k,i} = 0 then. *)
            bc_rhs.{j} <- bc_rhs.{j} -. lap.{k,i} *. v;
            lap.{k,i} <- 0.;
          done;
          for k = max2 1 (i + band - nnodes) to band - 1 do
            let j = i + band - k in     (* below diag *)
            bc_rhs.{j} <- bc_rhs.{j} -. lap.{k,j} *. v;
            lap.{k,j} <- 0.;
          done;
          bc_rhs.{i} <- v;              (* on diag *)
          lap.{band, i} <- 1.;
          bc_dirichlet := i :: !bc_dirichlet
  done;
  { mesh = mesh;  area = area;  inner = inner;  lap = lap;
    bc_rhs = bc_rhs;
    bc_dirichlet = !bc_dirichlet }
;;

let solve lap =
  let lap_copy = Mat.create (Mat.dim1 lap.lap) (Mat.dim2 lap.lap) in
  fun b ->
    (* FIXME: should factorize the matrix once for all *)
    Array2.blit lap.lap lap_copy;
    List.iter (fun i -> b.{i} <- 0.) lap.bc_dirichlet;
    axpy b ~x:lap.bc_rhs;
    let rhs = reshape_2 (genarray_of_array1 b) (Array1.dim b) 1 in
    pbsv lap_copy rhs

let mesh lap = lap.mesh

let area lap t = lap.area.{t}

let nnodes lap = Mat.dim2 lap.inner

let inner lap (u: vec) (v: vec) = dot u (sbmv lap.inner v)

let norm2 lap u = inner lap u u
let norm lap u = sqrt(inner lap u u)

let dual lap ?y (u: vec) = sbmv ?y lap.inner u

let add_inner ?(alpha=1.) lap m =
  Mat.axpy m ~alpha ~x:lap.inner

let solve_with_bc lap m b =
  (* Modify the system to impose BC.  See [make] for further details. *)
  let nnodes = Mat.dim2 lap.inner in
  let band = Mat.dim1 lap.inner in
  let modify i =
    let v = lap.bc_rhs.{i} in
    for k = 1 + max2 0 (band - i) to band - 1 do
      let j = i - band + k in     (* above diag *)
      b.{j} <- b.{j} -. m.{k,i} *. v;
      m.{k,i} <- 0.;
    done;
    for k = max2 1 (i + band - nnodes) to band - 1 do
      let j = i + band - k in     (* below diag *)
      b.{j} <- b.{j} -. m.{k,j} *. v;
      m.{k,j} <- 0.;
    done;
    b.{i} <- v;
    m.{band, i} <- 1.;
  in
  List.iter modify lap.bc_dirichlet;
  let rhs = reshape_2 (genarray_of_array1 b) (Array1.dim b) 1 in
  pbsv m rhs


(* let eigenvalues lap = *)
(*   let im_copy = Mat.create (Mat.dim1 lap.im) (Mat.dim2 lap.im) in *)
(* sbev *)

(* Use the second order formula that estimates the integral of a
   function f over a triangle T as area/3 of the sum of the values of f
   in the middle of the triangle edges. *)
let nonlin ?y lap f =
  let y = match y with
    | None -> Vec.create (Mat.dim2 lap.mesh#point)
    | Some y -> y in
  fun u ->
    Array1.fill y 0.;
    let tr = lap.mesh#triangle in
    let pt = lap.mesh#point in
    let area = lap.area in
    for t = 1 to Array2.dim2 tr do
      let i1 = tr.{1,t}   and i2 = tr.{2,t}  and i3 = tr.{3,t} in
      let x1 = pt.{1,i1}  and y1 = pt.{2,i1} in
      let x2 = pt.{1,i2}  and y2 = pt.{2,i2} in
      let x3 = pt.{1,i3}  and y3 = pt.{2,i3} in
      let x12 = 0.5 *. (x1 +. x2)  and y12 = 0.5 *. (y1 +. y2) in
      let x13 = 0.5 *. (x1 +. x3)  and y13 = 0.5 *. (y1 +. y3) in
      let x23 = 0.5 *. (x2 +. x3)  and y23 = 0.5 *. (y2 +. y3) in
      let u12 = 0.5 *. (u.{i1} +. u.{i2})
      and u13 = 0.5 *. (u.{i1} +. u.{i3})
      and u23 = 0.5 *. (u.{i2} +. u.{i3}) in
      let w = area.{t} /. 6. in
      y.{i1} <- y.{i1} +. w *. (f x12 y12 u12 +. f x13 y13 u13);
      y.{i2} <- y.{i2} +. w *. (f x12 y12 u12 +. f x23 y23 u23);
      y.{i3} <- y.{i3} +. w *. (f x13 y13 u13 +. f x23 y23 u23);
    done;
    y

let integrate lap f u =
  let tr = lap.mesh#triangle in
  let pt = lap.mesh#point in
  let area = lap.area in
  let integ = ref 0. in
  for t = 1 to Array2.dim2 tr do
    let i1 = tr.{1,t}   and i2 = tr.{2,t}  and i3 = tr.{3,t} in
    let x1 = pt.{1,i1}  and y1 = pt.{2,i1} in
    let x2 = pt.{1,i2}  and y2 = pt.{2,i2} in
    let x3 = pt.{1,i3}  and y3 = pt.{2,i3} in
    let x12 = 0.5 *. (x1 +. x2)  and y12 = 0.5 *. (y1 +. y2) in
    let x13 = 0.5 *. (x1 +. x3)  and y13 = 0.5 *. (y1 +. y3) in
    let x23 = 0.5 *. (x2 +. x3)  and y23 = 0.5 *. (y2 +. y3) in
    let u12 = 0.5 *. (u.{i1} +. u.{i2})
    and u13 = 0.5 *. (u.{i1} +. u.{i3})
    and u23 = 0.5 *. (u.{i2} +. u.{i3}) in
    integ := !integ +. area.{t} /. 3. *. (f x12 y12 u12 +. f x13 y13 u13
                                         +. f x23 y23 u23);
  done;
  !integ

let hessian ?y lap f =
  let m = match y with
    | None -> Mat.create (Mat.dim1 lap.inner) (Mat.dim2 lap.inner)
    | Some y -> y in
  fun (u:vec) ->
    let band = Mat.dim1 m in
    Array2.fill m 0.;
    let tr = lap.mesh#triangle in
    let pt = lap.mesh#point in
    let area = lap.area in
    for t = 1 to Array2.dim2 tr do
      let i1 = tr.{1,t}   and i2 = tr.{2,t}  and i3 = tr.{3,t} in
      let x1 = pt.{1,i1}  and y1 = pt.{2,i1} in
      let x2 = pt.{1,i2}  and y2 = pt.{2,i2} in
      let x3 = pt.{1,i3}  and y3 = pt.{2,i3} in
      let x12 = 0.5 *. (x1 +. x2)  and y12 = 0.5 *. (y1 +. y2) in
      let x13 = 0.5 *. (x1 +. x3)  and y13 = 0.5 *. (y1 +. y3) in
      let x23 = 0.5 *. (x2 +. x3)  and y23 = 0.5 *. (y2 +. y3) in
      let u12 = 0.5 *. (u.{i1} +. u.{i2})
      and u13 = 0.5 *. (u.{i1} +. u.{i3})
      and u23 = 0.5 *. (u.{i2} +. u.{i3}) in
      let w = area.{t} /. 12. in
      (* Diagonal *)
      m.{band, i1} <- m.{band, i1} +. w *. (f x12 y12 u12 +. f x13 y13 u13);
      m.{band, i2} <- m.{band, i2} +. w *. (f x12 y12 u12 +. f x23 y23 u23);
      m.{band, i3} <- m.{band, i3} +. w *. (f x13 y13 u13 +. f x23 y23 u23);
      (* Above diag *)
      let j = band + i2 - i1 in
      m.{j, i1} <- m.{j, i1} +. w *. f x12 y12 u12;
      let j = band + i3 - i1 in
      m.{j, i1} <- m.{j, i1} +. w *. f x13 y13 u13;
      let j = band + i3 - i2 in
      m.{j, i2} <- m.{j, i2} +. w *. f x23 y23 u23;
    done;
    m

(* Use the formulas in "Symmetric quadrature rules on a triangle",
   S. Wandzurat* and H. Xiao, for a higher order integration scheme? *)

(* For P1 elements, the positive part can be computed node by node. *)
let pos u =
  for i = 1 to Vec.dim u do
    if u.{i} < 0. then u.{i} <- 0.
  done

let neg u =
  for i = 1 to Vec.dim u do
    if u.{i} > 0. then u.{i} <- 0.
  done

let vec_of_fun lap f =
  let pt = lap.mesh#point in
  let marker = lap.mesh#point_marker in
  let nnodes = Mat.dim2 pt in
  let u = Vec.create nnodes in
  for i = 1 to nnodes do
    if marker.{i} = 0 then u.{i} <- f pt.{1,i} pt.{2,i}
    else u.{i} <- lap.bc_rhs.{i}
  done;
  u
