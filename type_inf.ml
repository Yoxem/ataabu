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

let type_inference_table_list = ref [type_inference_table;];;
let reference_list = ref [[""]];;

let ref_list = ref[["c"; "d"];[];["a"; "b"]];; (*prepared for testing*)




let rec line_infer l ref_list type_list =
  match l with
    (*lambda *)
    (*TODO: lambda as a input argument*)
    | Parser.Ls ([Parser.Item(Tokenizer.Token("lambda", "ID")); Parser.Ls([Parser.Item(Tokenizer.Token("%args", "ID"));
      Parser.Ls([Parser.Item(Tokenizer.Token(typ, "ID"));Parser.Item(Tokenizer.Token(id, "ID"))])]); Parser.Ls(body)]) ->
      (let new_ref_list = ref ([ ]::!ref_list) in
      let new_type_inference_table_block =  Hashtbl.create 10 in
      let real_typ = Simp(String.uppercase_ascii typ) in
      let _ = Hashtbl.add new_type_inference_table_block id real_typ in
      let new_type_inference_table_list = ref (new_type_inference_table_block::!type_list) in
      let a = (List.map (fun x -> line_infer x new_ref_list new_type_inference_table_list) body) in
      let last_of_a = (List.nth a ((List.length a)-1)) in
      Imply(real_typ, last_of_a))

  (*definition *)
  | Parser.Ls ([Parser.Item(Tokenizer.Token("%def", "ID")); typ; Parser.Item(Tokenizer.Token(id, "ID")); rhs]) ->
    (let rhs_type = line_infer rhs ref_list type_list in
    (* let _ = print_string ("~~~" ^ id ^ "~~") in 
    let _ = (List.map print_string (List.nth !ref_list 0))in 
    let _ = print_string "~~~ " in *)
    if List.mem id (List.nth !ref_list 0) then
      TypeError ("duplicated defined.")
    else(
    match typ with
    | Parser.Item(Tokenizer.Token(simp, "ID")) ->
      if equal_type (Simp(String.uppercase_ascii simp)) rhs_type
        then
          let _ = (ref_list := (id::List.hd !ref_list)::(List.tl !ref_list)) in
          let _ = Hashtbl.add type_inference_table id (Simp(String.uppercase_ascii simp)) in
          Void
        else TypeError ("lhs and rhs type unmatched.")
      (*lambda : todo*)
    | _ -> Void))
  | Parser.Ls ([Parser.Item(Tokenizer.Token("%apply", "ID")); caller; callee]) ->
    ( let caller_type = line_infer caller ref_list type_list in
  let callee_type = line_infer callee ref_list type_list in 

    (match caller_type with
      | Imply(arg_type,ret_type) -> if equal_type callee_type arg_type then ret_type else TypeError ("arg type and callee type unmatched.")
      | _ -> TypeError ("arg type and callee type unmatched.")
      ))
  (* operator *)
  | Parser.Ls ([Parser.Item(Tokenizer.Token(opr, "OP")); lhs; rhs]) ->
    let lhs_type = line_infer lhs ref_list type_list in
    let rhs_type = line_infer rhs ref_list type_list in
    let op_type = Hashtbl.find (List.nth !type_inference_table_list (List.length !type_inference_table_list -1)) opr in
    (match op_type with
    | OpType(op_lhs_type, op_rhs_type, op_res_type) ->
      if (equal_type lhs_type op_lhs_type) && (equal_type op_rhs_type rhs_type) then
        op_res_type
      else
        TypeError ("op_type unmatched: " ^  opr)
    | _ -> TypeError "operator unfound" )
  | Parser.Ls (Parser.Item(Tokenizer.Token(opr, "OP"))::rest) -> TypeError "operator unfound"
  (*variable*)
  | Parser.Item(Tokenizer.Token(var, "ID")) ->
    (let id_type = ref (Some(TypeError "unfound")) in
    let checked = ref false in
    let safe_find x var = (try Some(Hashtbl.find x var) with Not_found -> None) in
    let checker = (fun x ->
      (match safe_find x var with
        | None -> ()
        | tp_found -> if !checked == false then let _ = (id_type := tp_found) in let _ = (checked := true) in () else ())) in
    let _ = List.iter checker !type_list in
    ( match !id_type with
    | None -> TypeError ("inference get signal undound")
    | Some(n) -> 
        let _ = (ref_list := (var::(List.hd !ref_list))::(List.tl !ref_list)) in
        n )
    )
  | Parser.Item(Tokenizer.Token(const, typ)) -> Simp(typ)
  | _ -> TypeError "other type error";;



let type_infer parseoutput = 
  let result  =match parseoutput with
    | Parser.Success(Ls(lines), remained_tokens) ->
      List.map (fun x -> line_infer x reference_list type_inference_table_list) lines
    | _ -> [Void] in
  
  let print_err typ = match typ with 
  | TypeError(msg) -> let _ = print_string ("TypeError:" ^ msg) in ()
  | _ -> () in
  List.map print_err result

(*    let type_infer parseoutput = 
      match parseoutput with
        | Parser.Success(Ls(lines), remained_tokens) ->
          List.map (fun x -> print_string (type2string x ^ ";")) (List.map (fun x -> line_infer x ref_list type_inference_table_list) lines)
        | _ -> List.map (fun x -> print_string (type2string x))  [Void]*)


