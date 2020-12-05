let modname_of_filename s =
  Filename.basename s |>
  Filename.chop_extension |>
  String.capitalize_ascii  

include Peahell.Make(struct

    let name = "incrml"

    type input = Parsetree.structure_item list
    
    let options = []

    type environment = Env.t
    let initial_environment = Env.empty
                                
    let read_more str = 
      let i = ref (String.length str - 1) in
      while !i >= 0 && List.mem str.[!i] [' '; '\n'; '\t'; '\r'] do decr i done ;
      !i < 1 || (str.[!i] <> ';' || str.[!i - 1] <> ';')

    let file_parser =
      let f _name lexbuf =
        let r =
          Peahell.Input.wrap (Parser.implementation Lexer.token) lexbuf
        in
        r
      in
      Some f
    let toplevel_parser = None

    let expect_parser = 
      let f _name lexbuf =
        Peahell.Input.wrap (Parser.expect_file Lexer.token) lexbuf        
      in
      Some ( "(*EXPECT", "*)", f)
    
    let exec fmt _import env l =
      let f env i =
        let i, env = Typing.type_item env i in
        Peahell.Report.fprintf fmt "%a@?" Printer.signature_item i;
        env
      in
      List.fold_left f env l

  end)
