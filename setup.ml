(* OASIS_START *)
(* DO NOT EDIT (digest: 9852805d5c19ca1cb6abefde2dcea323) *)
(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2011-2013, Sylvain Le Gall                                   *)
(* Copyright (C) 2008-2011, OCamlCore SARL                                    *)
(*                                                                            *)
(* This library is free software; you can redistribute it and/or modify it    *)
(* under the terms of the GNU Lesser General Public License as published by   *)
(* the Free Software Foundation; either version 2.1 of the License, or (at    *)
(* your option) any later version, with the OCaml static compilation          *)
(* exception.                                                                 *)
(*                                                                            *)
(* This library is distributed in the hope that it will be useful, but        *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY *)
(* or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more         *)
(* details.                                                                   *)
(*                                                                            *)
(* You should have received a copy of the GNU Lesser General Public License   *)
(* along with this library; if not, write to the Free Software Foundation,    *)
(* Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA              *)
(******************************************************************************)

let () =
  try
    Topdirs.dir_directory (Sys.getenv "OCAML_TOPLEVEL_PATH")
  with Not_found -> ()
;;
#use "topfind";;
#require "oasis.dynrun";;
open OASISDynRun;;

(* OASIS_STOP *)

open OASISTypes

let setup_t =
  (* Unless one installs on OSX with "brew install ocaml --with-x11",
     the [Graphics] module will not be present. *)
  let has_graphics = try BaseCheck.package_version "graphics"; true
                     with Failure _ -> false in
  (* Change [setup_t] to set the default value of the flag "graphics"
     to [true]. *)
  if has_graphics then
    let change_flag = function
      | Flag(cs, flag) when cs.cs_name = "has_graphics" ->
         let flag = { flag with
                      flag_default = [(OASISExpr.EBool true, true)] } in
         Flag(cs, flag)
      | section -> section in
    let package =
      { setup_t.package with
        sections = List.map change_flag setup_t.package.sections } in
    { setup_t with package }
  else
    setup_t

let () =
  BaseSetup.setup setup_t
