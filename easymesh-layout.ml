(* Interface to easymesh for LAYOUT_layout matrices
 ***********************************************************************)

open Bigarray
open Printf
open Mesh

(* Write the [pslg] to the channel [fh]. *)
let output_pslg_LAYOUT fh (pslg: LAYOUT_layout Mesh.t) area =
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
  fprintf fh "%i\n" BA_NCOL(pt); (* number of nodes *)
  for i = BA_FIRST to BA_LASTCOL(pt) do
    (* EasyMesh expects indexes in 0 .. nnodes-1 *)
    fprintf fh "%i: %.13g %.13g %.13g %i\n"
      BA_OFIDX(i) BA_GET(pt,BA_FIRST,i) BA_GET(pt,BA_SECOND,i)
      area (pt_marker i)
  done;
  (* Save segments *)
  fprintf fh "%i\n" BA_NCOL(seg); (* number of segments *)
  for i = BA_FIRST to BA_LASTCOL(seg) do
    fprintf fh "%i: %i %i %i\n"
      BA_OFIDX(i)
      BA_OFIDX(BA_GET(seg,BA_FIRST,i)) BA_OFIDX(BA_GET(seg,BA_SECOND,i))
      (seg_marker i)
  done


(* FIXME: comments are possible in the files *)
(* FIXME: if a file does not exists, return empty array? *)
(* [read_LAYOUT fname] reads the collection of filenames [fname].n,
   [fname].e and [fname].s and creates a mesh struture with LAYOUT
   layout.  This function can throw a variety of exceptions depending
   of what goes wrong.  *)
let read_LAYOUT fname =
  (* Read nodes *)
  let fh = open_in (fname ^ ".n") in
  let nnodes = int_of_string(input_line fh) in
  let pt = BA_CREATE float64 2 nnodes in
  let pt_mark = Array1.create int LAYOUT_layout nnodes in
  for i = BA_FIRST to BA_LASTCOL(pt) do
    Scanf.fscanf fh " %i: %g %g %i"
      (fun i x y m ->
         let i = BA_TOIDX(i) in
         BA_GET(pt,BA_FIRST,i) <- x;
         BA_GET(pt,BA_SECOND,i) <- y;
         pt_mark.{i} <- m)
  done;
  close_in fh;
  (* Read triangles *)
  let fh = open_in (fname ^ ".e") in
  let n = int_of_string(input_line fh) in
  let tr = BA_CREATE int 3 n
  and tr_nb = BA_CREATE int 3 n in
  for i = BA_FIRST to BA_LASTCOL(tr) do
    Scanf.fscanf fh " %i: %i %i %i %i %i %i %_i %_i %_i %_f %_f %_i"
      (fun e i j k ei ej ek ->
         let e = BA_TOIDX(e) in
         BA_GET(tr,BA_FIRST,e) <- BA_TOIDX(i);
         BA_GET(tr,BA_SECOND,e) <- BA_TOIDX(j);
         BA_GET(tr,BA_THIRD,e) <- BA_TOIDX(k);
         BA_GET(tr_nb,BA_FIRST,e) <- BA_TOIDX(ei);
         BA_GET(tr_nb,BA_SECOND,e) <- BA_TOIDX(ej);
         BA_GET(tr_nb,BA_THIRD,e) <- BA_TOIDX(ek);
      )
  done;
  close_in fh;
  try
    (* Read edges, if file exists *)
    let fh = open_in (fname ^ ".s") in
    let n = int_of_string(input_line fh) in
    let edge = BA_CREATE int 2 n
    and edge_marker = Array1.create int LAYOUT_layout n in
    for i = BA_FIRST to BA_LASTCOL(edge) do
      Scanf.fscanf fh " %i: %i %i %_i %_i %i"
        (fun s c d m ->
           let s = BA_TOIDX(s) in
           BA_GET(edge,BA_FIRST,s) <- BA_TOIDX(c);
           BA_GET(edge,BA_SECOND,s) <- BA_TOIDX(d);
           edge_marker.{s} <- m;
        )
    done;
    close_in fh;
    { (Mesh.empty LAYOUT_layout) with
        point = pt;
        point_marker = pt_mark;
        triangle = tr;
        neighbor = tr_nb;
        edge = edge;
        edge_marker = edge_marker;
    }
  with Sys_error _ ->
    { (Mesh.empty LAYOUT_layout) with
        point = pt;
        point_marker = pt_mark;
        triangle = tr;
        neighbor = tr_nb;
    }

