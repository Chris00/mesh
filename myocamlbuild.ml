(* OASIS_START *)
(* OASIS_STOP *)
(* Ocamlbuild_plugin.dispatch dispatch_default;; *)

open Ocamlbuild_plugin

let env = BaseEnvLight.load() (* setup.data *)

let libtriangle = bool_of_string(BaseEnvLight.var_get "libtriangle" env)
;;
dispatch
  (MyOCamlbuildBase.dispatch_combine [
    dispatch_default;
    begin function
    | After_rules ->
      let includes = ["meshFC.ml"; "easymeshFC.ml"; "mesh_displayFC.ml";
                      "mesh_level_curvesFC.ml"; "mesh_triangleFC.ml";
                      "triangulate_stub.c" ] in
      let includes =
        if libtriangle then includes
        else "triangle/triangle.c" :: "triangle/triangle.h" :: includes in
      let includes = List.map (fun f -> "src" / f) includes in
      dep ["ocaml"; "ocamldep"] includes;
      dep ["ocaml"; "compile"] includes;

      let pp_src = S[A"-ppopt"; A"-Isrc"] in
      flag ["ocaml"; "compile";  "pkg_camlp4.macro"] & pp_src;
      flag ["ocaml"; "ocamldep"; "pkg_camlp4.macro"] & pp_src;
      flag ["ocaml"; "doc";      "pkg_camlp4.macro"] & pp_src;
      flag ["ocaml"; "infer_interface"; "pkg_camlp4.macro"] pp_src;
    | _ -> ()
    end;
  ]);;
