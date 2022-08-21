open Printf;;

let gensym =
  let x = ref 0 in 
    fun () ->
    (let tmp = Printf.sprintf "sym%d" (!x) in
    x := !x + 1;
    tmp)

(*
let ex_token_list = Tokenizer.total_parser "lambda(x){x;}(12);";;
Parser.print_parseoutput (Parser.stmts ex_token_list);;*)

let ex_token_list2 = Tokenizer.total_parser "12;7;int a = 1 + 2;lambda(int x){x + a;};9;";;
let ex_parseoutput2 = Parser.stmts ex_token_list2;;

let infering_result = Type_inf.type_infer ex_parseoutput2;; (*type infering*)
let ex_parseoutput3 = Parser.Ls(Closure_conv.closure_conv_main ex_parseoutput2);; (*closure_conversion*) 


print_string (Parser.ast2string ex_parseoutput3);;




let list_mut = ref (Parser.Ls([]));;
let main_str = ref "";;


let rec codegen ast_tree =
  match ast_tree with
  | Parser.Ls(ls_inner) -> let _ = (List.map codegen_aux ls_inner) in true
  | Parser.Item(_) -> codegen ast_tree
  | Parser.ASTFail -> false

and codegen_aux ast_tree = 
  match ast_tree with
  | Parser.Item(Tokenizer.Token(num, "INT")) ->
      let sym = (gensym ()) in
      let fmt = format_of_string "
          Object %s;
          %s.type =\"int\";
          %s.value.inte = %d;\n"  in
      let item_str = Printf.sprintf fmt sym sym sym (int_of_string num) in
      main_str := !(main_str) ^ item_str;
      sym
  | Parser.Item(Tokenizer.Token(var, "ID"))  -> main_str := !(main_str) ^ "\t" ^ var  ^ ";"; var
  | Parser.Ls([Parser.Item(Tokenizer.Token("+", "OP")); x; y]) ->
    let sym = (gensym ()) in
    let lhs = codegen_aux x in
    let rhs = codegen_aux y in
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
    Object* %s = %s;\n\n" in 
    let item_str = (Printf.sprintf fmt clo_fv result_rhs) in
    let _ = (main_str := !(main_str) ^ item_str ) in
      ""
    | Parser.Ls([Parser.Item(Tokenizer.Token("%def", "ID")); typ; Parser.Item(Tokenizer.Token(lhs, "ID")); y]) ->
      let rhs = codegen_aux y in
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
  | _ -> "0";;


print_string !main_str;;

let output_var_string =  codegen ex_parseoutput3;;

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
  } Object;

  int main() {
    %s
    return 0;}

    " in
    Printf.sprintf preamble str;;

print_string (print_main !main_str);;

(*
Printf.printf "%s" (gensym ());;
Printf.printf "%s" (gensym ());;
Printf.printf "%s" (gensym ());;*)