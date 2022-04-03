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

let ex_token_list2 = Tokenizer.total_parser "12;a;7;";;
let ex_parseoutput2 = Parser.stmts ex_token_list2;;
let ex_asttree2 = match ex_parseoutput2 with
  | Success(x, remained) -> x
  | Fail -> ASTFail;;

(* print_string (Parser.ast2string ex_asttree2);; *)

let list_mut = ref (Parser.Ls([]));;


let rec codegen ast_tree list = 
  match ast_tree with
  | Parser.Ls(ls_inner) -> List.fold_left (fun x y -> x ^ y) "" (List.map (fun i -> codegen i list) ls_inner)
  | Parser.Item(Tokenizer.Token(num, "INT")) -> let sym = (gensym ()) in let fmt = format_of_string "Object *%s = malloc(sizeof(Object));
  (%s->type) =\"int\";
  (%s->cont).i = %d;\n"  in Printf.sprintf fmt sym sym sym (int_of_string num)
  | Parser.Item(Tokenizer.Token(var, "ID"))  -> var ^ ";\n"
  | _ -> "";;

let output_var_string =  (codegen ex_asttree2 list_mut);;

let print_main str = 
  let preamble = format_of_string
    "#include<stdio.h>;
    typedef struct Object Object;
    typedef union ObjCont{
      int i;
      float f;
      Object* ptr;} ObjCont;
    
    typedef struct Object
    {char* type;
     ObjCont cont;
     } Object;
    
    int main(void){
    %s;
    return 0;
    }
    " in
    Printf.sprintf preamble str;;

print_string (print_main output_var_string);;

(*
Printf.printf "%s" (gensym ());;
Printf.printf "%s" (gensym ());;
Printf.printf "%s" (gensym ());;*)