module SS = Set.Make(String);;
open Printf;;

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
      let new_bound_var_frame = ref [] in
      let list_body =  find_free_var body new_bound_var_frame in 
      let list_arg = find_free_var arg new_bound_var_frame in 
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



(*
  let ex_token_list = Tokenizer.total_parser "int a = 12 ; int d = 16; lambda(int b){a + b + d;};20;";;
  let ex_parseoutput = Parser.stmts ex_token_list;;

  Type_inf.type_infer ex_parseoutput;; *)



let genclosure =
  let x = ref 0 in 
    fun () ->
    let tmp = Printf.sprintf "clos%d" (!x) in
    let _ = (x := !x + 1) in
    tmp;;
  
(* Parser.print_parseoutput ex_parseoutput;; *)

let rec get_index_aux ls item idx =
  if idx == (List.length ls) then -1
  else
    (if (List.nth ls idx) == item then idx
    else get_index_aux ls item (idx+1))

let get_index ls item = 
  if List.mem item ls then
    get_index_aux ls item 0
  else -1


  let rec replacing_vars ln fv clos_sym =
    match ln with
    | Parser.Ls([Parser.Ls(list)]) ->  replacing_vars (Parser.Ls(list)) fv clos_sym
    | Parser.Ls(list) -> Parser.Ls(List.map (fun x -> replacing_vars x fv clos_sym) list)
    | Parser.Item(Tokenizer.Token(id, typ)) ->
      if (List.mem id fv) then
        (*let _ = print_string ("上大人" ^ id ^ "孔乙己") in *)

      (let index = get_index fv id in
      let sym_name = Printf.sprintf "fv[%d]" index in
      Parser.Item(Tokenizer.Token(sym_name, "ID")))
      else ln
    | _ -> ln





let rec closure_conv_replacing fv line = 
  let _ = List.map print_string fv in
  let tmp_list1 = List.map (fun var -> Parser.Item(Tokenizer.Token(var, "ID"))) fv in
  let fv_list = Parser.Ls(Parser.Item(Tokenizer.Token("%struct", "ID"))::tmp_list1) in
  match line with
  | Parser.Ls([Parser.Item(Tokenizer.Token("lambda", "ID")); Parser.Ls(args); Parser.Ls(body)]) -> 
    let closure_symbol = (genclosure ()) in
    let def_closure_list = Parser.Ls([Parser.Item(Tokenizer.Token("%def", "ID"));
                                      Parser.Item(Tokenizer.Token("STRUCT", "ID"));
                                      Parser.Item(Tokenizer.Token(closure_symbol, "ID"));
                                      fv_list]) in
    let replaced_body = List.map (fun l -> replacing_vars l fv closure_symbol) body in
    let temp = Parser.Ls([Parser.Item(Tokenizer.Token("Object*", "ID")); Parser.Item(Tokenizer.Token(closure_symbol,"ID"))]) in
    let replaced_lambda = Parser.Ls([Parser.Item(Tokenizer.Token("lambda", "ID")); Parser.Ls(args @ [temp]); Parser.Ls(replaced_body)]) in
    let return_result =  Parser.Ls([def_closure_list; replaced_lambda]) in
    return_result
  | Parser.Ls([Parser.Item(Tokenizer.Token("%apply" , "ID")); caller; callee]) ->
    let caller_new = closure_conv_replacing fv caller in 
    let callee_new = closure_conv_replacing fv callee in 
    (match caller_new with
    | Parser.Ls([closure_struct; closure_main]) ->
      (match callee_new with 
      | Parser.Ls([callee_struct; callee_main]) -> Parser.Ls([closure_struct; callee_struct;
        Parser.Ls([Parser.Item(Tokenizer.Token("%apply" , "ID")); closure_main; callee_main])])
      | _ -> Parser.Ls([closure_struct;Parser.Ls([Parser.Item(Tokenizer.Token("%apply" , "ID")); closure_main; callee_new])]))
    | _ -> line)
  | _ -> line



  

  let closure_conv_aux2 parseoutput = 
    (match parseoutput with
      | Parser.Success(Ls(lines), remained_tokens) ->
        (let free_var = ref [] in
        List.map (fun ln -> let fv = find_free_var ln free_var in
                            closure_conv_replacing fv ln) lines)
      | _ -> []);;

let closure_conv_main input =
  let middle = closure_conv_aux2 input in
  let rec modifier ls  =
  match ls with
  | Parser.Ls([Parser.Ls(Parser.Item(Tokenizer.Token("%def", "ID"))::Parser.Item(Tokenizer.Token("STRUCT", "ID"))::rs1 ); rs2 ])::rs3 ->
    Parser.Ls(Parser.Item(Tokenizer.Token("%def", "ID"))::Parser.Item(Tokenizer.Token("STRUCT", "ID"))::rs1)::rs2::rs3
  | hd::rs ->hd::(modifier rs)
  | _ -> ls in
  modifier middle;;

  (*
    List.map (fun x -> print_string (Parser.ast2string x)) (closure_conv_main ex_parseoutput);;*)