open Hashtbl;;
type token = Tokenizer.token;;
type ast_tree = Parser.ast_tree;;


type type_ = 
  | Void
  | Simp of string
  | Imply of (type_ * type_)
  | OpType of  (type_ * type_ *  type_)
  | TypeError of string;;

let rec type2string typ =
  match typ with
  | Void -> "Void"
  | Simp(a)-> "Unit(" ^ a ^ ")"
  | Imply(a,b) -> "(" ^ type2string(a) ^ "->" ^ type2string(b) ^ ")"
  | OpType(a,b,c) -> "(" ^ type2string(a) ^ "," ^ type2string(b) ^ "->" ^ type2string(c) ^ ")"
  | TypeError(msg) -> "TypeError: " ^ msg;;



let rec equal_type x y =
  match x with
  | Void -> (match y with | Void -> true | _ -> false)
  | Simp(a)-> (match y with | Simp(b) -> a = b | _ -> false)
  | Imply(a,b) -> (match y with | Imply(x,y) -> (equal_type a x) && (equal_type b y) | _ -> false)
  | OpType(a,b,c) -> (match y with | OpType(x,y,z) -> (equal_type a x) && (equal_type b y) && (equal_type c z) | _ -> false)
  | TypeError(msg1) -> (match y with | TypeError(msg2) -> msg1 = msg2 | _ -> false);;



let type_inference_table = Hashtbl.create 10;;

Hashtbl.add type_inference_table "+" (OpType(Simp("INT") , Simp("INT" ) , Simp("INT")));;
Hashtbl.add type_inference_table "-" (OpType(Simp("INT"), Simp("INT") , Simp("INT")));;
Hashtbl.add type_inference_table "*" (OpType(Simp("INT") , Simp("INT"),  Simp("INT")));;




let ex_token_list2 = Tokenizer.total_parser "a; 3; int a = 15; 1; a;";;
let ex_parseoutput2 = Parser.stmts ex_token_list2;;

let rec line_infer = fun l ->
  match l with
  | Parser.Ls ([Parser.Item(Tokenizer.Token("%def", "ID")); typ; Parser.Item(Tokenizer.Token(id, "ID")); rhs]) ->
    (let rhs_type = line_infer rhs in
    match typ with
    | Parser.Item(Tokenizer.Token(simp, "ID")) ->
      if equal_type (Simp(String.uppercase_ascii simp)) rhs_type
        then
          let _ = Hashtbl.add type_inference_table id (Simp(String.uppercase_ascii simp)) in
          Void
        else TypeError ("lhs and rhs type unmatched.")
      (*lambda : todo*)
    | _ -> Void)

  | Parser.Ls ([Parser.Item(Tokenizer.Token(opr, "OP")); lhs; rhs]) ->
    let lhs_type = line_infer lhs in
    let rhs_type = line_infer rhs in
    let op_type = Hashtbl.find type_inference_table opr in
    (match op_type with
    | OpType(op_lhs_type, op_rhs_type, op_res_type) ->
      if (equal_type lhs_type op_lhs_type) && (equal_type op_rhs_type rhs_type) then
        op_res_type
      else
        TypeError ("op_type unmatched: " ^  opr)
    | _ -> TypeError "operator unfound" )
  | Parser.Ls (Parser.Item(Tokenizer.Token(opr, "OP"))::rest) -> TypeError "operator unfound"
  | Parser.Item(Tokenizer.Token(var, "ID")) -> Hashtbl.find type_inference_table var
  | Parser.Item(Tokenizer.Token(const, typ)) -> Simp(typ)
  | _ -> TypeError "other type error";;


Parser.print_parseoutput ex_parseoutput2;;

match ex_parseoutput2 with
  | Success(Ls(lines), remained_tokens) -> List.map (fun x -> print_string (type2string x)) (List.map line_infer lines)
  | _ -> List.map (fun x -> print_string (type2string x))  [Void]


