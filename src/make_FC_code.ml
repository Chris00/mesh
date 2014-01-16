(* Generate the layout dependent files by textual substitution. *)

#load "str.cma";;

let string_of_file fn =
  let buf = Buffer.create 4096 in
  let fh = open_in fn in
  let s = String.create 4096 in
  let read = ref 1 in (* enter the loop *)
  while !read > 0 do
    read := input fh s 0 4096;
    Buffer.add_substring buf s 0 !read;
  done;
  Buffer.contents buf

let write_ro fn s =
  (try Sys.remove fn with _ -> ());
  let fh = open_out_gen [Open_wronly; Open_creat; Open_trunc] 0o444 fn in
  output_string fh s;
  close_out fh


let include_re = Str.regexp "INCLUDE(\\([^()]+\\))"

let rec tr_include s =
  let inc s =
    let fn = Filename.concat "src" (Str.matched_group 1 s) in
    tr_include (string_of_file fn) in
  Str.global_substitute include_re inc s

let arg =
  "\\([.A-Za-z0-9_ ]+\\|[.A-Za-z0-9_ ]*([.A-Za-z0-9_ ]*)[.A-Za-z0-9_ ]*\\)"

let tr_fortran =
  let tr = [
    "LAYOUT", "fortran_layout";
    "DEFAULT_SWITCHES", "";
    "FST", "1";
    "SND", "2";
    "THIRD", "3";
    "TO_FORTRAN(\\([^()]*\\))", "\\1";
    (* FORTRAN view taken as default: *)
    ("CREATE_VEC(" ^ arg ^ ", *" ^ arg ^ ")",
     "Array1.create (\\1) fortran_layout (\\2)");
    ("CREATE_MAT(" ^ arg ^ ", *" ^ arg ^ ", *\\(" ^ arg ^ "\\))",
     "Array2.create (\\1) fortran_layout (\\2) (\\3)");
    "GET(\\([^,]*\\), *\\([^,]*\\), *\\([^()]*\\))", "\\1.{\\2,\\3}";
    "LINE_COL(\\([^,]*\\), *\\([^()]*\\))", "(\\1)(\\2)";
    "NCOLS(\\([^()]+\\))", "Array2.dim2(\\1)";
    "NROWS(\\([^()]+\\))", "Array2.dim1(\\1)";
    "LASTCOL(\\([^()]+\\))", "Array2.dim2(\\1)";
    "LASTROW(\\([^()]+\\))", "Array2.dim1(\\1)";
    "LASTEL(\\([^()]+\\))", "Array1.dim(\\1)";
    "COLS", "dim2";
    "ROWS", "dim1";
    "OF_IDX(\\([^()]+\\))", "\\1 - 1"; (* Fortran -> Easymesh indexes *)
    "TO_IDX(\\([^()]+\\))", "\\1 + 1"; (* Easymesh -> Fortran *)
  ] in
  List.map (fun (re, s) -> (Str.regexp re, s)) tr

let tr_c =
  let tr = [
    "LAYOUT", "c_layout";
    "DEFAULT_SWITCHES", "z";
    "FST", "0";
    "SND", "1";
    "THIRD", "2";
    "TO_FORTRAN(\\([^()]*\\))", "(\\1) + 1";
    (* C matrices are transpose of FORTRAN: *)
    ("CREATE_VEC(" ^ arg ^ ", *" ^ arg ^ ")",
     "Array1.create (\\1) c_layout (\\2)");
    ("CREATE_MAT(" ^ arg ^ ", *" ^ arg ^ ", *\\(" ^ arg ^ "\\))",
     "Array2.create (\\1) c_layout (\\3) (\\2)");
    "GET(\\([^,]*\\), *\\([^,]*\\), *\\([^()]*\\))", "\\1.{\\3,\\2}";
    "LINE_COL(\\([^,]*\\), *\\([^()]*\\))", "(\\2)(\\1)";
    "NCOLS(\\([^()]+\\))", "Array2.dim1(\\1)";
    "NROWS(\\([^()]+\\))", "Array2.dim2(\\1)";
    "LASTCOL(\\([^()]+\\))", "Array2.dim1(\\1) - 1";
    "LASTROW(\\([^()]+\\))", "Array2.dim2(\\1) - 1";
    "LASTEL(\\([^()]+\\))", "Array1.dim(\\1) - 1";
    "COLS", "dim1";
    "ROWS", "dim2";
    "OF_IDX(\\([^()]+\\))", "\\1"; (* Easymesh *)
    "TO_IDX(\\([^()]+\\))", "\\1";
  ] in
  List.map (fun (re, s) -> (Str.regexp re, s)) tr


let gen_FC ~mod_name fn_base =
  let s = string_of_file (fn_base ^ "FC.ml") in
  let s = tr_include s in
  let replace s (re,tr) = Str.global_replace re tr s in
  let m = (Str.regexp "MOD", mod_name) in
  write_ro (fn_base ^ "F.ml") (List.fold_left replace s (m :: tr_fortran));
  write_ro (fn_base ^ "C.ml") (List.fold_left replace s (m :: tr_c))

let rm_gen fn_base =
  (try Sys.remove (fn_base ^ "F.ml") with _ -> ());
  (try Sys.remove (fn_base ^ "C.ml") with _ -> ())


let filenames =
  (* Filename, module name *)
  [ "src/mesh_triangle", "Mesh";
    "src/mesh", "Mesh";
    "src/mesh_display", "Mesh_display";
    "src/easymesh", "Easymesh";
  ]

let () =
  let clean = ref false in
  let specs = [
    "--clean", Arg.Set clean, " Remove the generated .ml files";
  ] in
  Arg.parse (Arg.align specs) (fun _ -> raise(Arg.Bad "no anonymous arg"))
            "ocaml make_FC_code.ml";

  if !clean then
    List.iter (fun (fn, _) -> rm_gen fn) filenames
  else
    List.iter (fun (fn, mod_name) -> gen_FC fn ~mod_name) filenames
