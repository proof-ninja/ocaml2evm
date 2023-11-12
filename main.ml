module Compile_common = struct
  open Misc
  open Compenv

  type info = {
    source_file : string;
    module_name : string;
    output_prefix : string;
    env : Env.t;
    ppf_dump : Format.formatter;
    tool_name : string;
    native : bool;
  }

  let cmx i = i.output_prefix ^ ".cmx"
  let obj i = i.output_prefix ^ Config.ext_obj
  let cmo i = i.output_prefix ^ ".cmo"
  let annot i = i.output_prefix ^ ".annot"

  let with_info ~native ~tool_name ~source_file ~output_prefix ~dump_ext k =
    Compmisc.init_path ();
    let module_name = module_of_filename source_file output_prefix in
    Env.set_unit_name module_name;
    let env = Compmisc.initial_env () in
    let dump_file = String.concat "." [ output_prefix; dump_ext ] in
    Compmisc.with_ppf_dump ~file_prefix:dump_file @@ fun ppf_dump ->
    k
      {
        module_name;
        output_prefix;
        env;
        source_file;
        ppf_dump;
        tool_name;
        native;
      }

  (** Compile a .mli file *)

  let parse_intf i =
    Pparse.parse_interface ~tool_name:i.tool_name i.source_file
    |> print_if i.ppf_dump Clflags.dump_parsetree Printast.interface
    |> print_if i.ppf_dump Clflags.dump_source Pprintast.signature

  let typecheck_intf info ast =
    Profile.(record_call typing) @@ fun () ->
    let tsg =
      ast
      |> Typemod.type_interface info.env
      |> print_if info.ppf_dump Clflags.dump_typedtree Printtyped.interface
    in
    let sg = tsg.Typedtree.sig_type in
    if !Clflags.print_types then
      Printtyp.wrap_printing_env ~error:false info.env (fun () ->
          Format.(fprintf std_formatter)
            "%a@."
            (Printtyp.printed_signature info.source_file)
            sg);
    ignore (Includemod.signatures info.env ~mark:Mark_both sg sg);
    Typecore.force_delayed_checks ();
    Warnings.check_fatal ();
    tsg

  let emit_signature info ast tsg =
    let sg =
      let alerts = Builtin_attributes.alerts_of_sig ast in
      Env.save_signature ~alerts tsg.Typedtree.sig_type info.module_name
        (info.output_prefix ^ ".cmi")
    in
    Typemod.save_signature info.module_name tsg info.output_prefix
      info.source_file info.env sg

  let interface info =
    Profile.record_call info.source_file @@ fun () ->
    let ast = parse_intf info in
    if Clflags.(should_stop_after Compiler_pass.Parsing) then ()
    else
      let tsg = typecheck_intf info ast in
      if not !Clflags.print_types then emit_signature info ast tsg

  (** Frontend for a .ml file *)

  let parse_impl i =
    Pparse.parse_implementation ~tool_name:i.tool_name i.source_file
    |> print_if i.ppf_dump Clflags.dump_parsetree Printast.implementation
    |> print_if i.ppf_dump Clflags.dump_source Pprintast.structure

  let typecheck_impl i parsetree =
    parsetree
    |> Profile.(record typing)
         (Typemod.type_implementation i.source_file i.output_prefix
            i.module_name i.env)
    |> print_if i.ppf_dump Clflags.dump_typedtree
         Printtyped.implementation_with_coercion

  let implementation info ~backend =
    Profile.record_call info.source_file @@ fun () ->
    let exceptionally () =
      let sufs = if info.native then [ cmx; obj ] else [ cmo ] in
      List.iter (fun suf -> remove_file (suf info)) sufs
    in
    Misc.try_finally ?always:None ~exceptionally (fun () ->
        let parsed = parse_impl info in

        (* SCaml special handling *)
        (* let m = SCamlc.Preprocess.mapper in
           let parsed = m.Ast_mapper.structure m parsed in *)
        (if Clflags.(should_stop_after Compiler_pass.Parsing) then ()
         else
           let typed = typecheck_impl info parsed in
           if Clflags.(should_stop_after Compiler_pass.Typing) then ()
           else backend info typed);
        Warnings.check_fatal ())
end

module Compile = struct
  open Compile_common

  let tool_name = "scamlc"
  let with_info = Compile_common.with_info ~native:false ~tool_name
  let interface ~source_file:_ ~output_prefix:_ = assert false

  let implementation ~start_from ~source_file ~output_prefix =
    (* let backend info typed =
         SCamlc.SCamlComp.compile info.source_file info.output_prefix
           info.module_name typed
       in *)
    let backend = Ocamyul.backend in
    with_info ~source_file ~output_prefix ~dump_ext:"cmo" @@ fun info ->
    match (start_from : Clflags.Compiler_pass.t) with
    | Parsing -> Compile_common.implementation info ~backend
    | _ ->
        Misc.fatal_errorf "Cannot start from %s"
          (Clflags.Compiler_pass.to_string start_from)
end

let _ =
  Compile.implementation ~start_from:Parsing ~source_file:"test.ml"
    ~output_prefix:""
