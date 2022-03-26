open List;;


let token_list = Tokenizer.total_parser "lambda(x){let a = 2;

return a + x;};";;

let not_empty_token token = match token with
  | Tokenizer.Token( _ , token_type) -> match token_type with
    | "SPACE" -> false
    | "NL" -> false
    | _ -> true;;

let token_list_filtered = List.filter not_empty_token token_list

type parseoutput =
  | Fail
  | Success of  Tokenizer.token * ( Tokenizer.token list);;

let consume1token ls =
    match ls with
    | [] -> Fail
    | token :: token_rest -> Success ( token, token_rest);;

let parseoutput_list2string str token = 
  str ^ (Tokenizer.token_to_string token)

let rec parseoutput2string input =
  match input with
  | Fail ->  "Fail"
  | Success(tkn, tkn_ls) -> (Tokenizer.token_to_string tkn) ^ 
                              ":::" ^ List.fold_left parseoutput_list2string "" tkn_ls;;

let print_parseoutput input = print_string (parseoutput2string input);; 

let token_list_filtered = List.filter not_empty_token token_list in
    let cosumed = consume1token token_list_filtered in
    print_parseoutput cosumed;;