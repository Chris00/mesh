(* FORTRAN/C functions *)

(* Write the [pslg] to the channel [fh]. *)
let output_pslg fh (pslg: mesh) area =
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
  fprintf fh "%i\n" (NCOLS(pt)); (* number of nodes *)
  for i = FST to LASTCOL(pt) do
    (* EasyMesh expects indexes in 0 .. nnodes-1 *)
    fprintf fh "%i: %.13g %.13g %.13g %i\n"
      (OF_IDX(i)) (GET(pt,FST,i)) (GET(pt,SND,i))  area (pt_marker i)
  done;
  (* Save segments *)
  fprintf fh "%i\n" (NCOLS(seg)); (* number of segments *)
  for i = FST to LASTCOL(seg) do
    fprintf fh "%i: %i %i %i\n"
      (OF_IDX(i))  (OF_IDX(GET(seg,FST,i))) (OF_IDX(GET(seg,SND,i)))
      (seg_marker i)
  done

(* FIXME: comments are possible in the files *)
(* FIXME: if a file does not exists, return empty array? *)
(* [read_fortran fname] reads the collection of filenames [fname].n,
   [fname].e and [fname].s and creates a mesh struture with fortran
   layout.  This function can throw a variety of exceptions depending
   of what goes wrong.  *)
let read fname =
  (* Read nodes *)
  let fh = open_in (fname ^ ".n") in
  let nnodes = int_of_string(input_line fh) in
  let pt = CREATE_MAT(float64, 2, nnodes) in
  let pt_mark = CREATE_VEC(int, nnodes) in
  let sb = Scanning.from_channel fh in
  for i = FST to LASTCOL(pt) do
    bscanf sb " %i: %g %g %i" (fun i x y m ->
                                 let i = TO_IDX(i) in
                                 GET(pt, FST,i) <- x;
                                 GET(pt, SND,i) <- y;
                                 pt_mark.{i} <- m)
  done;
  close_in fh;
  (* Read triangles *)
  let fh = open_in (fname ^ ".e") in
  let n = int_of_string(input_line fh) in
  let tr = CREATE_MAT(int, 3, n)
  and tr_nb = CREATE_MAT(int, 3, n) in
  let sb = Scanning.from_channel fh in
  for i = FST to LASTCOL(tr) do
    bscanf sb " %i: %i %i %i %i %i %i %_i %_i %_i %_f %_f %_i"
      (fun e i j k ei ej ek ->
         let e = TO_IDX(e) in
         GET(tr, 1,e) <- TO_IDX(i);
         GET(tr, 2,e) <- TO_IDX(j);
         GET(tr, 3,e) <- TO_IDX(k);
         GET(tr_nb, 1,e) <- TO_IDX(ei);
         GET(tr_nb, 2,e) <- TO_IDX(ej);
         GET(tr_nb, 3,e) <- TO_IDX(ek);
      )
  done;
  close_in fh;
  try
    (* Read edges, if file exists *)
    let fh = open_in (fname ^ ".s") in
    let n = int_of_string(input_line fh) in
    let edge = CREATE_MAT(int, 2, n)
    and edge_marker = CREATE_VEC(int, n) in
    let sb = Scanning.from_channel fh in
    for i = FST to LASTCOL(edge) do
      bscanf sb " %i: %i %i %_i %_i %i" (fun s c d m ->
                                           let s = TO_IDX(s) in
                                           GET(edge, 1,s) <- TO_IDX(c);
                                           GET(edge, 2,s) <- TO_IDX(d);
                                           edge_marker.{s} <- m;
                                        )
    done;
    close_in fh;
    { (Mesh.empty LAYOUT) with
        point = pt;
        point_marker = pt_mark;
        triangle = tr;
        neighbor = tr_nb;
        edge = edge;
        edge_marker = edge_marker;
    }
  with Sys_error _ ->
    { (Mesh.empty LAYOUT) with
        point = pt;
        point_marker = pt_mark;
        triangle = tr;
        neighbor = tr_nb;
    }
