open Uchar;;
open StringLabels;;


type parseoutput =
    | Fail
    | Success of string * string;;

exception IndexException of string

let print_parse_output output =  match output with
    | Success (car, cdr) -> print_string ("Success(" ^ car ^ " " ^ cdr ^")\n")
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

let ( >>=* ) (input : parseoutput) parser = 
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

let ( >>=? ) (input : parseoutput) parser = 
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

let parse_id input = (input 
          >>= ((match_char "_") || (match_range "a" "z") ||(match_range "A" "Z"))
          >>=* ((match_char "_") || (match_range "0" "9") || (match_range "a" "z") ||(match_range "A" "Z")));;



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