let modname_of_filename s =
  Filename.basename s |>
  Filename.chop_extension |>
  String.capitalize_ascii  

include Peyhel.Make(struct

    let name = "incrml"

    type input = Parsetree.structure
    
    let options = []

    type environment = Env.t
    let initial_environment = Env.empty
                                
    let read_more str = 
      let i = ref (String.length str - 1) in
      while !i >= 0 && List.mem str.[!i] [' '; '\n'; '\t'; '\r'] do decr i done ;
      !i < 1 || (str.[!i] <> ';' || str.[!i - 1] <> ';')

    let file_parser =
      let f name lexbuf =
        let modname = modname_of_filename name in
        let r =
          Peyhel.Input.wrap (Parser.implementation Lexer.token) lexbuf
        in
        {Parsetree.str_self = modname; str_content = r}
      in
      Some f
    let toplevel_parser = None
    
    let exec _import env c =
      let s = Typing.type_structure env c in
      Peyhel.Report.printf "%a@." Printer.unit s.sig_content;
      Env.add_module s.sig_self Modules.(Core (Signature s)) env

  end)
