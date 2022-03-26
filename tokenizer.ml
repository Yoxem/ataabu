open Uchar;;
open List;;
open StringLabels;;
open Printf;;
(* TODO: Add parsing attribute and operator + - * / \n \r # = == < > ; ( ) [ ]*)

type tokenizeroutput =
    | Fail
    | Success of string * string;;

type token = Token of string * string;;

type aux_middle = Aux of tokenizeroutput * string


exception IndexException of string

let token_to_string token =  match token with
| Token (str, token_type) -> "Token(" ^ str ^ ", " ^ token_type ^")\n";;


let print_token token =  print_string (token_to_string token);;


let print_parse_output output =  match output with
    | Success (car, cdr) -> print_string ("Success(" ^ car ^ ", " ^ cdr ^")\n")
    | Fail -> print_string ("Fail!\n");;
  

let consume1char str =
  match str with
  | "" -> Fail
  | _ -> Success ((sub str 0 1), (sub str 1 ((length str) - 1)));;

let match_range min max = 
  fun input ->
    if min > max then
     raise (IndexException (min ^ " should be not less than " ^ max))
  else
    let initial_result = consume1char input in
      match initial_result with
      | Fail -> Fail
      | Success (fst, rest) ->
        let fst_code = to_int (of_char (get fst 0)) in
        let max_code = to_int (of_char (get max 0)) in
        let min_code = to_int (of_char (get min 0)) in
        if fst_code >= min_code && fst_code <= max_code
          then Success (fst, rest)
          else Fail;;      

let match_char pattern = 
  fun input -> 
    let initial_result = consume1char input in
      match initial_result with 
        | Fail -> Fail
        | Success (fst, rest) ->
          if equal fst pattern
          then Success (fst, rest)
          else Fail;;

let  (>>=) input parser = 
  match input with 
    | Fail -> input
    | Success (fst , snd_rest) ->
      let middle = parser snd_rest in
          match middle with
          | Fail -> Fail
          | Success (snd, rest) -> 
            let fst_snd = fst ^ snd in
            Success (fst_snd, rest);;

let ( >>=* ) (input : tokenizeroutput) parser = 
  if input == Fail then
    Fail
  else
    let middle0 = input >>= parser in
    if middle0 == Fail then
      input
    else
      let rec parser_recursive i parser =
        let middle = i >>= parser in
        match middle with
          | Fail -> i
          | Success (a  , b) -> (parser_recursive middle parser) in
        (parser_recursive input parser);; 
    

let not_match_char pattern = 
  fun input -> 
    let initial_result = consume1char input in
      match initial_result with 
        | Fail -> Fail
        | Success (fst, rest) ->
          if not (equal fst pattern)
          then Success (fst, rest)
          else Fail;;

let ( >>=? ) (input : tokenizeroutput) parser = 
  let middle = input >>= parser in
    match middle with
      | Fail -> input
      | Success (a  , b) -> middle;;

let ( || ) parser1  parser2 = 
    fun input ->
      let middle1 = parser1 input in
        match middle1 with
          | Success (_ , _) -> middle1
          | Fail ->
            let middle2 = parser2 input in
            match middle2 with 
              | Fail -> Fail
              | Success (_ , _) -> middle2;;


let ( ||** ) parser1  parser2 = 
  fun input ->
    let middle1 = parser1 input in
      match middle1 with
        | Aux (Success (_ , _), _) -> middle1
        | Aux(Fail, _) ->
          let middle2 = parser2 input in
            middle2;;








let parse_int input = (input 
                        >>=* ((match_char "+") || (match_char "-")))
                        >>= (match_range "0" "9")
                        >>=* (match_range "0" "9");;

let parse_float input = (input 
                        >>=* ((match_char "+") || (match_char "-")))
                        >>= (match_range "0" "9")
                        >>=* (match_range "0" "9")
                        >>= (match_char ".")
                        >>= (match_range "0" "9")
                        >>=* (match_range "0" "9");;

(*concat 2 parser
let ( >> ) parser1 parser2 = 
  fun input ->
    if input == Fail then
      Fail
    else
      let middle1 = parser1 input in
      match middle1 with
      | Fail -> Fail
      | Success (_ , _) -> parser2 input *)

let inside_quote_mark  = ((fun i -> Success("", i)  >>= (match_char "\\") >>= (match_char "\"")) ||  (fun i -> Success("", i)  >>= (not_match_char "\""))) ;;

let parse_string input = (input 
                         >>= (match_char "\"")) 
                        >>=* (inside_quote_mark)
                        >>= (match_char "\"")  ;;

let parse_operator input = input >>= ((match_char "+") || (match_char "-") || (match_char "*") || (match_char "/") || (match_char "%"));;

let parse_number_mark input = input >>= (match_char "#");;

let parse_equal input = input >>= (match_char "==");;

let parse_assign input = input >>= (match_char "=");;

let parse_semicolon input = input >>= (match_char ";");;

let parse_parenthesis input = input >>= ((match_char "(")|| (match_char ")"));;

let parse_bracket input = input >>= ((match_char "[")|| (match_char "]"));;

let parse_brace input = input >>= ((match_char "{")|| (match_char "}"));;

let parse_newline input = input >>= (match_char "\n");;

let parse_spaces input = input >>= (match_char " ") >>=* (match_char " ");;







let parse_id input = (input 
          >>= ((match_char "_") || (match_range "a" "z") ||(match_range "A" "Z"))
          >>=* ((match_char "_") || (match_range "0" "9") || (match_range "a" "z") ||(match_range "A" "Z")));;

let rec total_parser_aux input list = 
  match input with
   | Success(_,"") -> list
   | _ -> 
    let initial = ((fun i -> Aux ((parse_id i), "ID"))
                ||** (fun i -> Aux ((parse_operator i) ,"OP"))
                ||** (fun i -> Aux ((parse_int i) ,"INT"))
                ||** (fun i -> Aux ((parse_float i) ,"FLO"))
                ||** (fun i -> Aux ((parse_number_mark i) ,"NUM_MRK"))
                ||** (fun i -> Aux ((parse_brace i) ,"BRACE"))
                ||** (fun i -> Aux ((parse_assign i), "ASSIGN"))
                ||** (fun i -> Aux ((parse_bracket i) ,"BRACK"))
                ||** (fun i -> Aux ((parse_parenthesis i) ,"PAREN"))
                ||** (fun i -> Aux ((parse_semicolon i), "SEMICO"))
                ||** (fun i -> Aux ((parse_newline i), "NL"))
                ||** (fun i -> Aux ((parse_spaces i) ,"SPACE")))
                 input in
    match initial with
      | Aux (Fail, _) -> let _ =  print_string "Error" in []
      | Aux (Success(matched, remained), token_type) -> total_parser_aux (Success("", remained)) (append list [Token(matched, token_type );]);;


let rec total_parser input = total_parser_aux (Success("", input)) [];;




(* tests

List.iter (print_token) (total_parser "lambda(x){let a = 2;

return a + x;};");;
List.iter (print_token) (total_parser "12+中34;");;

print_parse_output (parse_id (Success ("", "_")));;
print_parse_output (parse_id (Success ("", "_abc12c")));;
print_parse_output (parse_id (Success ("", "_9")));;
print_parse_output (parse_id (Success ("", "a_9A")));;
print_parse_output (parse_id (Success ("", "if")));;
print_parse_output (parse_id (Success ("", "Class")));;
print_parse_output (parse_id (Success ("", "BIGLETTER123__")));;
print_parse_output (parse_id (Success ("", "12a")));;






print_string  ("Test 5\n");;
print_parse_output  (((Success ("", "+1234a"))  >>=? (match_char "+")) >>=* (match_range "0" "9"));;
print_parse_output  (((Success ("", "1234a"))  >>=? (match_char "+")) >>=* (match_range "0" "9"));;
print_parse_output  (((Success ("", "-1234a"))  >>=? (match_char "+")) >>=* (match_range "0" "9"));;
print_parse_output  ((Success ("", "-1234a"))  >>=* (match_range "0" "9"));;


print_string  ("Test 6\n");;
print_parse_output  ((Success ("", "+1234a"))  >>= ( (match_char "+") || (match_char "-")));;
print_parse_output  ((Success ("", "-1234a"))  >>= ( (match_char "+") || (match_char "-")));;
print_parse_output  ((Success ("", "1234a"))  >>= ( (match_char "+") || (match_char "-")));;

print_string  ("Test 7\n");;
print_parse_output (parse_int (Success ("", "+1234a")));
print_parse_output (parse_int (Success ("", "-1234a")));
print_parse_output (parse_int (Success ("", "1234a")));
print_parse_output (parse_int (Success ("", "+a")));

print_string  ("Test 8\n");;
print_parse_output (parse_float (Success ("", "+1234.58a")));
print_parse_output (parse_float (Success ("", "-1234.58a")));
print_parse_output (parse_float (Success ("", "0.0a")));
print_parse_output (parse_float (Success ("", "+0.58a")));
print_parse_output (parse_float (Success ("", "0.58a")));
print_parse_output (parse_float (Success ("", "-0.58a")));
print_parse_output (parse_float (Success ("", "1234.8a")));
print_parse_output (parse_float (Success ("", "1234a")));
print_parse_output (parse_float (Success ("", "+1234a")));
print_parse_output (parse_float (Success ("", "-1234a")));

print_string  ("Test 9\n");;
(* print_parse_output (inside_quote_mark (Success ("", "abc"))); *)
(* print_parse_output (inside_quote_mark (Success ("", "\"abc"))); *)
(* print_parse_output (inside_quote_mark (Success ("", "\\\"abc"))); *)
print_parse_output (parse_string (Success ("","\"123\"")));;
print_parse_output (parse_string (Success ("","\"12\\\"3\"")));;
print_parse_output (parse_string (Success ("","\"\\\"\\\"\"")));;
print_parse_output (parse_string (Success ("","\"\"")));;

*)