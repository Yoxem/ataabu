open Printf;;
open Int;;
open StringLabels;;

let counter = ref 0;;

let gensym =
    fun () ->
    (let tmp = Printf.sprintf "sym%d" (!counter) in
    counter := !counter + 1;
    tmp)

(*
let ex_token_list = Tokenizer.total_parser "lambda(x){x;}(12);";;
Parser.print_parseoutput (Parser.stmts ex_token_list);;*)

let ex_token_list2 = Tokenizer.total_parser "(lambda(int x){x + 2;}(20));";;
let ex_parseoutput2 = Parser.stmts ex_token_list2;;

let infering_result = Type_inf.type_infer ex_parseoutput2;; (*type infering*)
let ex_parseoutput3 = Parser.Ls(Closure_conv.closure_conv_main ex_parseoutput2);; (*closure_conversion*) 


print_string (Parser.ast2string ex_parseoutput3);;




let list_mut = ref (Parser.Ls([]));;
let main_str = ref "";;
let lambda_counter = ref 1;;

let get_args_sym_string x =
  match x with
  | Parser.Ls([Parser.Item(Tokenizer.Token(arg_typ, "ID")); Parser.Item(Tokenizer.Token(arg_sym, "ID"))]) -> arg_sym
  | _ -> ""
                            


let rec codegen ast_tree main_str =
  match ast_tree with
  | Parser.Ls(ls_inner) -> let a = (List.map (fun x ->  codegen_aux x main_str) ls_inner) in (!main_str, (List.hd (List.rev a)))
  | Parser.Item(x) -> let a =  codegen_aux ast_tree main_str in (a, a)
  | Parser.ASTFail -> ("", "")

and codegen_aux ast_tree main_str= 
  match ast_tree with
  | Parser.Ls([Parser.Item(Tokenizer.Token("%apply", "ID")); caller; callee ]) ->
    let caller_side = codegen_aux caller main_str in 
    let callee_side = codegen_aux callee main_str in
    let res_sym = gensym () in
    let fmt = format_of_string "
          Object %s;
          %s = %s.value.func(%s, %s.free_var);
          "  in
    let item_str = Printf.sprintf fmt res_sym res_sym caller_side callee_side caller_side in
    main_str := !(main_str) ^ item_str;
    res_sym

  | Parser.Item(Tokenizer.Token(num, "INT")) ->
      let sym = (gensym ()) in
      let fmt = format_of_string "
          Object %s;
          %s.type =\"int\";
          %s.value.inte = %d;\n"  in
      let item_str = Printf.sprintf fmt sym sym sym (int_of_string num) in
      main_str := !(main_str) ^ item_str;
      sym
  | Parser.Ls([Parser.Item(Tokenizer.Token("lambda", "ID")); Parser.Ls(args_id::args); body ]) ->
    let current_lambda_counter = !lambda_counter in
    let _ = lambda_counter := !lambda_counter + 1 in
    let args_str_array = List.map get_args_sym_string args in
    let arg_str = List.hd args_str_array in
    let function_str = ref "" in
    let (body_string, get_return_id) = codegen body function_str in

    let sym_lambda = gensym () in
    let sym_closure = gensym () in
    let return_str = "return " ^ get_return_id ^ ";" in
    let fmt = format_of_string "
    Object %s (Object %s, Object* fv){
      %s
      %s
    }
    
    " in
    let item_str_tmp = Printf.sprintf fmt sym_lambda arg_str body_string return_str in

    let closure_str_fmt  = format_of_string
    "
    %s

    Object %s;
    %s.type= \"func\";
    %s.value.func = &%s;
    %s.free_var = clos%d ;
    " in
    let item_str = Printf.sprintf closure_str_fmt item_str_tmp sym_closure sym_closure sym_closure sym_lambda sym_closure current_lambda_counter in
    main_str := !(main_str) ^ item_str ;
    sym_closure

  | Parser.Item(Tokenizer.Token(var, "ID"))  -> main_str := !(main_str) ^ "\t" ^ var  ^ ";"; var
  | Parser.Ls([Parser.Item(Tokenizer.Token("+", "OP")); x; y]) ->
    let sym = (gensym ()) in
    let lhs = codegen_aux x main_str in
    let rhs = codegen_aux y main_str in
    let fmt = format_of_string
      "
      Object %s;
      %s.type = %s.type;
      if (%s.type = \"int\"){
        %s.value.inte = %s.value.inte + %s.value.inte;}
    else if (%s.type = \"flo\"){
      %s.value.doub = %s.value.doub + %s.value.doub;
    }
      %s;\n" in
      let item_str = (Printf.sprintf fmt sym sym lhs lhs sym lhs rhs sym sym lhs rhs sym) in
      let _ = (main_str := !(main_str) ^ item_str ) in
      sym
    
  | Parser.Ls([Parser.Item(Tokenizer.Token("%def", "ID")); Parser.Item(Tokenizer.Token("STRUCT", "ID"))  ;
    Parser.Item(Tokenizer.Token(clo_fv, "ID")) ; Parser.Ls(Parser.Item(Tokenizer.Token("%struct", "ID"))::fv_list)]) ->
    (*let fv_list = List.tl fv_ls in (*fv = free variable*)*)
    let fv_string_list = List.map (fun x -> match x with
                                            |Parser.Item(Tokenizer.Token(x, "ID"))-> x
                                            |_ -> "")
                                            fv_list in
    let result_rhs = "{" ^ (List.fold_right (fun x y -> x ^ ", " ^ y) fv_string_list "") ^ "}" in
    let fmt = format_of_string "
    Object %s[] = %s;\n\n" in 
    let item_str = (Printf.sprintf fmt clo_fv result_rhs) in
    let _ = (main_str := !(main_str) ^ item_str ) in
      ""
    | Parser.Ls([Parser.Item(Tokenizer.Token("%def", "ID")); typ; Parser.Item(Tokenizer.Token(lhs, "ID")); y]) ->
      let rhs = codegen_aux y main_str in
      let fmt = format_of_string 
      "
      Object %s;
      %s.type = %s.type;
      if (%s.type = \"int\"){
        %s.value.inte = %s.value.inte;}
      else if (%s.type = \"flo\"){
        %s.value.doub = %s.value.doub;
    }
    else{
      %s.value.func = %s.value.func;
    }\n" in
    let item_str = (Printf.sprintf fmt lhs lhs rhs lhs lhs rhs lhs lhs rhs lhs rhs) in
    let _ = (main_str := !(main_str) ^ item_str ) in
    ""
  | Parser.Ls([Parser.Ls(inner)]) -> (codegen_aux (Parser.Ls(inner)) main_str)
  | _ -> "0";;

let (output_var_string, _) =  codegen ex_parseoutput3 main_str;;

let print_main str = 
  let preamble = format_of_string
  "
  #include <stdio.h>
  #include <stdlib.h>
  
typedef struct Object Object;

typedef union ObjectValue{
  int inte;
  double doub;
  char *str;
  Object (*func) (Object, Object*);
  
  } ObjectValue;

typedef struct Object{
  char* type;
  ObjectValue value;
  Object* free_var;
  } Object;

  int main() {
    %s
    return 0;}

    " in
    Printf.sprintf preamble str;;

(*print_string output_var_string;; *)

print_string (print_main output_var_string);;

(*
Printf.printf "%s" (gensym ());;
Printf.printf "%s" (gensym ());;
Printf.printf "%s" (gensym ());;*)