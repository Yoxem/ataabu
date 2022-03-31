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

let rec correct_list ls = 
  match ls with
  | Ls([lhs; Ls(op::rhs)]) -> Ls(op::lhs::[(correct_list(Ls(rhs)))])
  | Ls([Item(Token(id, typ))]) -> Item(Token(id, typ))
  | _ -> ls

let rec add_sub_rest token_list =
  let wrapper = Success(Ls([]),  token_list) in 
    let result1 = wrapper >>= ((match_token_name_type "+" "OP") || (match_token_name_type "-" "OP"))  >>= (match_token_type "INT") >>= add_sub_rest in
      match result1 with
      | Success(Ls(_), remained_tokens) -> result1
      | _ -> wrapper

let rec add_sub token_list =
  let wrapper = Success(Ls([]),  token_list) in 
    let result1 = wrapper >>= (match_token_type "INT") >>= add_sub_rest in
      match result1 with
      | Success(Ls(x), remained) -> Success((correct_list (Ls(x))), remained)
      | _ -> result1

          (*   
let add_sub token_list =
  let result1 = Success(Ls([]), token_list) >>= (match_token_type "INT")
                >>= (match_token_name_type "+" "OP") >>= (match_token_type "INT") in
    match result1 with
    | Success(Ls(ast_list), remained_tokens) -> Success(Ls[(nth ast_list 1); (nth ast_list 0) ; (nth ast_list 2)], remained_tokens)
    | _ -> result1;;*)

let ex_token_list = Tokenizer.total_parser "2-3";;

(*  List.iter Tokenizer.print_token ex_token_list;;  *)

print_parseoutput (add_sub ex_token_list);;
print_string "\n\n";;

let ex_token_list = Tokenizer.total_parser "2";;

(*  List.iter Tokenizer.print_token ex_token_list;;  *)



print_parseoutput (add_sub ex_token_list);;
print_string "\n\n";;


let ex_token_list = Tokenizer.total_parser "5+6-7";;

(*  List.iter Tokenizer.print_token ex_token_list;;  *)



print_parseoutput (add_sub ex_token_list);;
