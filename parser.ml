open List;;
open StringLabels;;

type token = Tokenizer.token;;

let token_list = Tokenizer.total_parser "lambda(x){let a = 2;

return a + x;};";;

type ast_tree = 
  | ASTFail
  | Item of token
  | Ls of (ast_tree list);;

let ast_example = Ls ([Item(Tokenizer.Token ("12", "INT"));Item(Tokenizer.Token ("+", "OP")); Item(Tokenizer.Token ("2", "INT")); ]);;


let not_empty_token token = match token with
  | Tokenizer.Token( _ , token_type) -> match token_type with
    | "SPACE" -> false
    | "NL" -> false
    | _ -> true;;

let token_list_filtered = List.filter not_empty_token token_list



type parseoutput =
  | Fail
  | Success of  ast_tree * ( token list);;





let consume1token ls =
    match ls with
    | [] -> Fail
    | token :: token_rest -> Success ( Item(token), token_rest);;


let match_token_type token_type = 
  fun token_ls -> 
    let initial_result = consume1token token_ls in
      match initial_result with 
        | Success (Item(Token(_ , type_name)) , rest) ->
          if equal type_name token_type
          then initial_result
          else Fail
        | Fail -> Fail
        | _ -> Fail;;

let match_token_name_type token_name token_type = 
  fun token_ls -> 
    let initial_result = consume1token token_ls in
      match initial_result with 
        | Success (Item(Token( nm , tp)) , rest) ->
          if ((equal token_name nm) && (equal token_type tp))
          then initial_result
          else Fail
        | Fail -> Fail
        | _ -> Fail;;
    

let parseoutput_list2string str token = 
  str ^ (Tokenizer.token_to_string token);;

let rec ast2string ast_tree = 
  match ast_tree with
  | ASTFail -> "ASTFail"
  | Item(token) -> Tokenizer.token_to_string token
  | Ls(ast) -> "Ls(" ^ (List.fold_left (fun str ast -> str ^ " " ^ (ast2string ast)) "" ast)  ^ ")";;

let rec parseoutput2string input =
  match input with
  | Fail ->  "Fail"
  | Success(matched_ast, tkn_remained_ls) -> ast2string matched_ast ^ 
                              ":::" ^ List.fold_left parseoutput_list2string "" tkn_remained_ls


let print_parseoutput input = print_string (parseoutput2string input);; 

let token_list_filtered = List.filter not_empty_token token_list;;

let cosumed = (match_token_name_type "lambda" "IS")  token_list_filtered in
  print_parseoutput cosumed;;

let (>>=) parseoutput parser_unit = 
  match parseoutput with
  | Fail -> Fail
  | Success(matched1 , remained1) ->
    let result = parser_unit remained1 in
      match result with
      | Fail -> Fail
      | Success (Ls([]) , remained2) ->
        parseoutput
      | Success (matched2 , remained2) ->
        match matched1 with
          | Ls(matched_list1) -> Success (Ls(append matched_list1 [matched2]), remained2)
          | Item(matched_item1) -> Success (Ls(append [matched1] [matched2]), remained2)
          | ASTFail -> Fail;;

let (||) parser_unit1 parser_unit2 = 
  fun parseoutput ->
    let middle1 = parser_unit1 parseoutput in
    match middle1 with
      | Success (_ , _) -> middle1
      | Fail ->
        let middle2 = parser_unit2 parseoutput in
        match middle2 with 
          | Fail -> Fail
          | Success (_ , _) -> middle2;;










let rec ( >>=* ) input parser = 
  if input == Fail then
    Fail
  else
    let middle0 = input >>= parser in
    match middle0 with
    | Success(Ls(_), remained_tokens) -> middle0 >>=* parser
    | _ -> input    

let rec correct_list ls = 
  match ls with
  | Ls([lhs; Ls(op::rhs)]) -> Ls(op::lhs::[(correct_list(Ls(rhs)))])
  | Ls([Item(Token(id, typ))]) -> Item(Token(id, typ))
  | Ls([Ls(lst)]) -> (correct_list (Ls(lst)))
  | _ -> ls


(*item2 = (expr) | int | id *) 
let rec item2 token_list =
  let wrapper = Success(Ls([]),  token_list) in 
    let result1 = wrapper >>= ((fun i -> Success(Ls([]), i) >>= (match_token_name_type "(" "PAREN") >>= expr >>=(match_token_name_type ")" "PAREN"))
                                || (match_token_type "INT")
                                || (match_token_type "ID")) in
      match result1 with
      | Success(Ls([Ls([Item(Token("(", "PAREN")); x; Item(Token(")", "PAREN"))])]), remained) -> Success((correct_list x), remained)
      | Success(ls, remained) -> Success((correct_list ls), remained)
      | _ -> result1

and args token_list = 
  let wrapper = Success(Ls([]),  token_list) in
  let result1 = wrapper >>= (match_token_name_type "(" "PAREN") >>=* (fun i -> Success(Ls([]), i) >>= (match_token_type "ID") >>=*(fun i -> Success(Ls([]), i) >>= (match_token_type "COMMA") >>= (match_token_type "ID"))) >>= (match_token_name_type ")" "PAREN") in
    match result1 with
    | Success(Ls(left_paren::Ls(other)::righ_paren), remained) ->
                      let remove_comma = fun ls -> match ls with Ls([l; x]) -> x | _ -> ls in
                      let other_removed_comma = List.map remove_comma other in
                      Success(Ls(Item(Token("%args", "ID"))::other_removed_comma), remained)
    |  Success(Ls(left_paren::righ_paren), remained) -> Success(Ls([Item(Token("%args", "ID"))]), remained)
    | _ -> result1                  
    (*| Success(Ls(Item(Token("(", "PAREN"))::first_args::[Item(Token(")", "PAREN"))]), y) ->
                          Success(Ls([first_args]), y)
    | Success(Ls(Item(Token("(", "PAREN"))::first_args::rest_lst), y) ->
                          let lst_without_r_paren = filter (fun x ->
                                                              match x with
                                                              | Item(_) -> false
                                                              | _ -> true) rest_lst in
                          let remove_comma = fun ls -> match ls with Ls([Item(Token(",", "COMMA")); x]) -> x | _ -> ls in
                          let lst_removed_comma = List.map remove_comma lst_without_r_paren in
                          Success(Ls(Item(Token("%args", "ID"))::first_args::lst_removed_comma), y) *)
    | _ -> result1  

 (*factor = item2 | lambda  "(" args ")" {stmts} *) 
and item token_list =
  let wrapper = Success(Ls([]),  token_list) in 
  let result1 = wrapper >>= ((fun i -> Success(Ls([]), i) >>= (match_token_name_type "lambda" "ID") >>= args
  >>= (match_token_name_type "{" "BRACE") >>= stmts >>= (match_token_name_type "}" "BRACE"))
                              || fun i -> Success(Ls([]), i) >>= item2) in
    match result1 with
    | Success(Ls([Ls(Item(Token("lambda", "ID"))::args::l_brace::[Item(Token("}", "BRACE"))])]) , remained) ->
                                        Success(Ls([Item(Token("lambda", "ID"));args;Ls([])]), remained)
    | Success(Ls([Ls(Item(Token("lambda", "ID"))::args::l_brace::body::r_brace)]) , remained) ->
                                        Success(Ls([Item(Token("lambda", "ID"));args;body]), remained)
    | _ ->  result1


and factor_more_callees token_list = 
  let wrapper = Success(Ls([]),  token_list) in 
  let result1 = wrapper >>= (match_token_name_type "(" "PAREN") >>= item >>=*
    (fun i -> Success(Ls([]), i) >>= (match_token_type "COMMA") >>= item)
    >>=(match_token_name_type ")" "PAREN") in
    match result1 with
    | Success(Ls(Item(Token("(", "PAREN"))::first_callee::rest_lst), y) ->
                          let lst_without_r_paren = filter (fun x ->
                                                              match x with
                                                              | Item(_) -> false
                                                              | _ -> true) rest_lst in
                          let remove_comma = fun ls -> match ls with Ls([Item(Token(",", "COMMA")); x]) -> x | _ -> ls in
                          let lst_removed_comma = List.map remove_comma lst_without_r_paren in
                          Success(Ls(Item(Token("%callee", "ID"))::first_callee::lst_removed_comma), y)
    | _ -> result1

 (*factor = item | item  "(" morecollee ")" *) 
and factor token_list =
  let wrapper = Success(Ls([]),  token_list) in 
    let result1 = wrapper >>= ((fun i -> Success(Ls([]), i) >>= item >>= (match_token_name_type "(" "PAREN")  >>= (match_token_name_type ")" "PAREN"))
                                || (fun i -> Success(Ls([]), i) >>= item >>= factor_more_callees)
                                || item) in
      match result1 with
      | Success(ASTFail, _) -> result1
      | Fail -> Fail
      | Success(Item _, _) -> result1
      | Success(Ls(other), remained) ->
        let result2 = Success((correct_list (Ls(other))), remained) in
        match result2 with
        | Success(Ls[caller; Item(Token("(", "PAREN")); Item(Token(")", "PAREN"))], remained) ->
          Success(Ls[Item(Token("%apply", "ID")); caller], remained)
        | Success(Ls[caller; Item(Token("(", "PAREN")); callee ; Item(Token(")", "PAREN"))], remained) ->
          Success(Ls[Item(Token("%apply", "ID")); caller; callee], remained)
        | Success(Ls(Item(Token("%callee", "ID"))::op::rest), remained) -> let l1 = Item(Token("%apply", "ID"))::op::rest in
                                                    let l2 = List.filter (fun x -> match x with Ls([]) -> false | _ -> true) l1 in
                                                    Success(Ls(l2), remained)
        | _ -> result2


      (*
(* ( */ factor) *)
let rec factor_rest token_list =
  let wrapper = Success(Ls([]),  token_list) in 
    let result1 = wrapper >>= ((match_token_name_type "*" "OP") || (match_token_name_type "/" "OP"))  >>= (match_token_type "INT") >>= term_rest in
      match result1 with
      | Success(Ls(_), remained_tokens) -> result1
      | _ -> wrapper *)



  (* ( */ factor) *)
and term_rest token_list =
  let wrapper = Success(Ls([]),  token_list) in 
    let result1 = wrapper >>= ((match_token_name_type "*" "OP") || (match_token_name_type "/" "OP"))  >>= factor >>= term_rest in
      match result1 with
      | Success(Ls(_), remained_tokens) -> result1
      | _ -> wrapper

(*term = factor ( */ factor)* *) 
and term token_list =
  let wrapper = Success(Ls([]),  token_list) in 
    let result1 = wrapper >>= factor >>= term_rest in
      match result1 with
      | Success(Ls(x), remained) -> Success((correct_list (Ls(x))), remained)
      | _ -> result1

(* (+- term) *)
and expr_rest token_list =
  let wrapper = Success(Ls([]),  token_list) in 
    let result1 = wrapper >>= ((match_token_name_type "+" "OP") || (match_token_name_type "-" "OP"))  >>= term >>= expr_rest in
      match result1 with
      | Success(Ls([Item(x) ; Ls(lists)]), remained_tokens) -> Success(Ls((Item(x)::lists)), remained_tokens)
      | Success(Ls(_), remained_tokens) -> result1
      | _ -> wrapper

(*expr = term (+- term)* *) 
and expr token_list =
  let wrapper = Success(Ls([]),  token_list) in 
    let result1 = wrapper >>= term >>= expr_rest in
      match result1 with
      | Success(Ls(x), remained) -> Success((correct_list (Ls(x))), remained)
      | _ -> result1

(* var_def = id id | expr ;*)
and var_def token_list =
  let wrapper = Success(Ls([]),  token_list) in 
    let result1 = wrapper >>= (match_token_type "ID") >>=  (match_token_type "ID") >>= (match_token_type "ASSIGN") >>= expr in
      match result1 with
      | Success(Ls(typ::var::assign::expr), remained_tokens) -> Success(Ls(Item(Token("%def", "ID"))::typ::var::expr), remained_tokens)
      | _ -> wrapper



(* one_statement = var_def | expr ;*)
and one_statement token_list =
  let token_list2 = List.filter (fun x -> match x with Tokenizer.Token(_, "SPACE") -> false | Tokenizer.Token(_, "NL") -> false | _ -> true) token_list in
  let wrapper = Success(Ls([]),  token_list2) in 
    let result1 = wrapper >>= ((fun i -> Success(Ls([]), i) >>= expr >>= (match_token_name_type ";" "SEMICO") )||(fun i -> Success(Ls([]), i) >>= var_def >>= (match_token_name_type ";" "SEMICO")))   in
      match result1 with
      | Success(Ls(lst), remained_tokens) ->  let lst2 = (correct_list (Ls(lst))) in
                                              let lst2_inner =  match lst2 with
                                                | Ls(lst2_inner) -> lst2_inner
                                                | _ -> [lst2] in
                                              let lst_remove_semicolon = List.filter (fun x -> match x with Item(Token(_, "SEMICO")) -> false | _ -> true) lst2_inner in
                                              Success((correct_list (Ls(lst_remove_semicolon))), remained_tokens)
      | _ -> result1

(* stmts = one_statement* *)
and stmts token_list =
let wrapper = Success(Ls([]),  token_list) in 
  let result1 = wrapper >>=* one_statement in
  match result1 with
  | Success(Ls(_), remained_tokens) -> result1
  | _ -> result1 

          (*   
let add_sub token_list =
  let result1 = Success(Ls([]), token_list) >>= (match_token_type "INT")
                >>= (match_token_name_type "+" "OP") >>= (match_token_type "INT") in
    match result1 with
    | Success(Ls(ast_list), remained_tokens) -> Success(Ls[(nth ast_list 1); (nth ast_list 0) ; (nth ast_list 2)], remained_tokens)
    | _ -> result1;;*)

let ex_token_list = Tokenizer.total_parser "2-3;";;

(*  List.iter Tokenizer.print_token ex_token_list;;  *)

print_parseoutput (one_statement ex_token_list);;
print_string "\n\n";;

let ex_token_list = Tokenizer.total_parser "(2);";;

(*  List.iter Tokenizer.print_token ex_token_list;;  *)



print_parseoutput (one_statement ex_token_list);;
print_string "\n\n";;


let ex_token_list = Tokenizer.total_parser "7/(5+6)*7;";;
print_parseoutput (stmts ex_token_list);;

print_string "\n\n";;
let ex_token_list = Tokenizer.total_parser "(7/(10-6)*a);";;
print_parseoutput (stmts ex_token_list);;

print_string "\n\n";;
let ex_token_list = Tokenizer.total_parser "a(b);";;
print_parseoutput (stmts ex_token_list);;

print_string "\n\n";;
let ex_token_list = Tokenizer.total_parser "a();";;
print_parseoutput (stmts ex_token_list);;

print_string "\n\n";;
let ex_token_list = Tokenizer.total_parser "a(b,c,a);";;
print_parseoutput (stmts ex_token_list);;

print_string "\n\n";;
let ex_token_list = Tokenizer.total_parser "int a = 2+3;a + b;";;
print_parseoutput (stmts ex_token_list);;

print_string "\n\n";;
let ex_token_list = Tokenizer.total_parser "lambda(a, b){12;};";;
print_parseoutput (stmts ex_token_list);;

print_string "\n\n";;
let ex_token_list = Tokenizer.total_parser "lambda(a){12;};";;
print_parseoutput (stmts ex_token_list);;

print_string "\n\n";;
let ex_token_list = Tokenizer.total_parser "lambda(){};";;
print_parseoutput (stmts ex_token_list);;