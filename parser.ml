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
  | Success of  (token list) * ( token list);;





let consume1token ls =
    match ls with
    | [] -> Fail
    | token :: token_rest -> Success ( [token;], token_rest);;


let match_token_type token_type = 
  fun token_ls -> 
    let initial_result = consume1token token_ls in
      match initial_result with 
        | Success ([Token(_ , type_name);] , rest) ->
          if equal type_name token_type
          then initial_result
          else Fail
        | Fail -> Fail
        | _ -> Fail;;

let match_token_name_type token_name token_type = 
  fun token_ls -> 
    let initial_result = consume1token token_ls in
      match initial_result with 
        | Success ([Token( nm , tp);] , rest) ->
          if ((equal token_name nm) && (equal token_type tp))
          then initial_result
          else Fail
        | Fail -> Fail
        | _ -> Fail;;
    

let parseoutput_list2string str token = 
  str ^ (Tokenizer.token_to_string token)

let rec parseoutput2string input =
  match input with
  | Fail ->  "Fail"
  | Success(tkn, tkn_ls) -> (List.fold_left parseoutput_list2string "" tkn) ^ 
                              ":::" ^ List.fold_left parseoutput_list2string "" tkn_ls;;


let print_parseoutput input = print_string (parseoutput2string input);; 

let token_list_filtered = List.filter not_empty_token token_list;;

let cosumed = (match_token_name_type "lambda" "IS")  token_list_filtered in
  print_parseoutput cosumed;;


let add_sub token_list = 
  let parseoutput = Success([], token_list) in
  match parseoutput with
  | Fail -> ASTFail
  | Success(matched_list_1, remained1) ->
    let res1 = (match_token_type "INT") remained1 in
    match res1 with
    | Fail -> ASTFail
    | Success([matched2], remained2) -> 
      let res2 = (match_token_name_type "+" "OP") remained2 in
      match res2 with
      | Fail -> ASTFail
      | Success([matched3], remained3) ->
        let res3 = (match_token_type "INT") remained3 in
        match res3 with
        | Fail -> ASTFail
        | Success([matched4], remained4)  -> 
          let _ = print_string (Tokenizer.token_to_string matched3) in
          let _ = print_string (Tokenizer.token_to_string matched2) in
          let _ = print_string (Tokenizer.token_to_string matched4) in
          Ls ([Item (matched3); Item (matched2); Item(matched4)])


let ex_token_list = Tokenizer.total_parser "2+3";;

List.iter Tokenizer.print_token ex_token_list;;

add_sub ex_token_list;;