module SS = Set.Make(String);;

(*let ex_token_list2 = Tokenizer.total_parser "int a = 15; 3;  1 + 1; a; (lambda(int y){lambda(int x){y+x;};}(12));";;
let ex_parseoutput2 = Parser.stmts ex_token_list2;;

Parser.print_parseoutput ex_parseoutput2;;

Type_inf.type_infer ex_parseoutput2;;*)

let rec find_free_var l bound_vars =
  match l with
  
  | Parser.Item(item) ->
    (*variable*)
    (match item with

    |Tokenizer.Token(var, "ID") ->
      (if List.mem var !bound_vars then
        []
      else
      [var])
    (* constant *)
    | Tokenizer.Token(_, "INT") -> []
    (* operator *)
    | Tokenizer.Token(_, "OP") -> []
    (* operator *)
    | Tokenizer.Token(_, other_op_type) -> [])
  | Parser.Ls(lst) -> (
    match lst with
    (*apply*)
    | [Parser.Item(Tokenizer.Token("%apply", "ID")); caller; callee] ->
        let list_combined = List.append (find_free_var caller bound_vars) (find_free_var callee bound_vars) in
        let empty_set = SS.empty in
        let set = List.fold_right SS.add list_combined empty_set in
        SS.elements set
  (*operator*)
    | [Parser.Item(Tokenizer.Token(op_name, "OP")); lhs; rhs] ->
      let list_combined = List.append (find_free_var lhs bound_vars) (find_free_var rhs bound_vars) in
      let empty_set = SS.empty in
      let set = List.fold_right SS.add list_combined empty_set in
      SS.elements set
    (*define*)
    | [Parser.Item(Tokenizer.Token("%def", "ID")); typ; Parser.Item(Tokenizer.Token(id, "ID")); inner] ->
      let list_inner =  find_free_var inner bound_vars in 
      let list_id = find_free_var (Parser.Item(Tokenizer.Token(id, "ID"))) bound_vars in 
      let empty_set = SS.empty in
      let set_inner = List.fold_right SS.add list_inner empty_set in
      let set_id = List.fold_right SS.add list_id empty_set in
      let set_result = SS.diff set_inner set_id in
      let _ = (bound_vars := id::!bound_vars) in
      SS.elements set_result
    (*lambda*)
    | [Parser.Item(Tokenizer.Token("lambda", "ID")); Parser.Ls([args_header; Parser.Ls([typ; arg])]); body] ->
      let list_body =  find_free_var body bound_vars in 
      let list_arg = find_free_var arg bound_vars in 
      let empty_set = SS.empty in
      let set_inner = List.fold_right SS.add list_body empty_set in
      let set_id = List.fold_right SS.add list_arg empty_set in
      let set_result = SS.diff set_inner set_id in
      SS.elements set_result

    | cmds ->
      let cmds_free_var = List.map (fun x -> find_free_var x bound_vars) cmds in
      let cmds_fv_flatten_ls = List.flatten cmds_free_var in
      let empty_set = SS.empty in
      let cmds_fv_flatten_ls_set = List.fold_right SS.add cmds_fv_flatten_ls empty_set in
      SS.elements cmds_fv_flatten_ls_set
  )
  | _ -> [];;




  let ex_token_list = Tokenizer.total_parser "int a = b ; a;lambda(int a){a + c;};d;";;
  let ex_parseoutput = Parser.stmts ex_token_list;;

  Type_inf.type_infer ex_parseoutput;;

  
  Parser.print_parseoutput ex_parseoutput;;

  let clusure_conv_main parseoutput = 
    (match parseoutput with
      | Parser.Success(Ls(lines), remained_tokens) ->
        (let free_var = ref [] in
        List.map (fun x -> find_free_var x free_var) lines)
      | _ -> []);;

List.map (fun ls -> List.map print_string ls) (clusure_conv_main ex_parseoutput);;