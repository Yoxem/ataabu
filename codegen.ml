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

let ex_token_list2 = Tokenizer.total_parser "12;7;3+5;";;
let ex_parseoutput2 = Parser.stmts ex_token_list2;;
let ex_asttree2 = match ex_parseoutput2 with
  | Success(x, remained) -> x
  | Fail -> ASTFail;;

(* print_string (Parser.ast2string ex_asttree2);; *)

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
      let fmt = format_of_string "Object *%s = malloc(sizeof(Object));
          (%s->type) =\"int\";
          (%s->cont).i = %d;\n"  in
      let item_str = Printf.sprintf fmt sym sym sym (int_of_string num) in
      main_str := !(main_str) ^ item_str;
      sym
  | Parser.Item(Tokenizer.Token(var, "ID"))  ->
    main_str := !(main_str) ^ var ^ ";\n";
    var
  | Parser.Ls([Parser.Item(Tokenizer.Token("+", "OP")); x; y]) ->
      let lhs = codegen_aux x in
      let rhs = codegen_aux y in
      let sym_array = gensym () in
      let sym_res = gensym () in
      let fmt = format_of_string
      "Object* %s[2];
      %s[0] = %s;
      %s[1] = %s;
      Object *%s = malloc(sizeof(Object));
      %s = add(%s, 0);" in
      let item_str = Printf.sprintf fmt sym_array sym_array lhs sym_array rhs sym_res sym_res sym_array in
      main_str := !(main_str) ^ item_str;
      sym_res
  | _ -> "0";;

let output_var_string =  (codegen ex_asttree2);;

let print_main str = 
  let preamble = format_of_string
  "
  #include <stdio.h>
  #include <stdlib.h>
  
  typedef struct Object Object;
  typedef Object* (Func)  (Object**, Object**);
  typedef struct Clos
  {
    Func* func;
    Object** free_vars;
  } Clos;
  
  
  typedef union ObjCont
  {
      int i;
      float f;
      Object * obj_ptr;
      Clos clos;
  }
  ObjCont;
  
  typedef struct Object
  {
      char *type;
      ObjCont cont;
  }
  Object;
  
  Object* add(Object** args, Object** free_vars){
      Object *result = malloc(sizeof(Object));
      (result->type) = \"int\";
      (result->cont).i = (args[0]->cont).i + (args[1]->cont).i;  
      return result;
    }
  
  int main(void)
  {
      Object *add_b78e8kc8 = malloc(sizeof(Object));
      (add_b78e8kc8->type) = \"closure\";
      (add_b78e8kc8->cont).clos.func = add;
      (add_b78e8kc8->cont).clos.free_vars = 0;
    %s;
    return 0;
    }
    " in
    Printf.sprintf preamble str;;

print_string (print_main !main_str);;

(*
Printf.printf "%s" (gensym ());;
Printf.printf "%s" (gensym ());;
Printf.printf "%s" (gensym ());;*)