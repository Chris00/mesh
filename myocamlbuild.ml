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
      let includes = ["triangulate_stub.c" ] in
      let includes =
        if libtriangle then includes
        else "triangle/triangle.c" :: "triangle/triangle.h" :: includes in
      let includes = List.map (fun f -> "src" / f) includes in
      dep ["ocaml"; "ocamldep"] includes;
      dep ["ocaml"; "compile"] includes;
    | _ -> ()
    end;
  ]);;
