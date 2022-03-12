use trees::tr;
use std::fs::File;
use std::io::prelude::*;


fn generate_exec_str(sexp_tree : & trees::Node<&str>) -> String{
    let mut iter = sexp_tree.iter();
    let mut result : String = "".to_string();
    let mut args_str : String = "".to_string();


    let func_str = iter.next().unwrap();


    let sexp_tree_degree = sexp_tree.degree();

    let mut next_subitem = iter.next();

    for _i in 0..(sexp_tree_degree - 1){
        args_str = format!("{},{}", args_str, convert_c_sexp_to_c(next_subitem.unwrap()));
        next_subitem = iter.next();
    }

    args_str = args_str[1..].to_string(); // remove the foremost ','.

    result = format!("{}({})", func_str , args_str);

    return result;
}

fn generate_incl_str(sexp_tree : & trees::Node<&str>) -> String{

    let mut iter = sexp_tree.iter();

    let importee_str = iter.next().unwrap();


    let result : String = format!("#include<{}>\n", importee_str);

    return result;

}

fn generate_stmts_str(sexp_tree : &trees::Node<&str>) -> String{

    let mut iter = sexp_tree.iter();
    let mut stmts_str : String = "".to_string();


    let sexp_tree_degree = sexp_tree.degree();

    let mut next_subitem = iter.next();

    for _i in 0..(sexp_tree_degree){
        stmts_str = format!("{}{};\n", stmts_str, convert_c_sexp_to_c(next_subitem.unwrap()));
        next_subitem = iter.next();
    }

    return stmts_str;

}

// define statement : int a = 12
// DEF("int" "a" 12)
fn generate_def_str(sexp_tree : &trees::Node<&str>) -> String{
    let mut iter = sexp_tree.iter();
    let _type = iter.next().unwrap();
    let type_name = convert_c_sexp_to_c(_type);

    let var = iter.next().unwrap();
    let var_str = convert_c_sexp_to_c(var);

    let rhs = iter.next().unwrap();
    let rhs_str = convert_c_sexp_to_c(rhs);

    let def_str = format!("{} {} = {};\n", type_name , var_str, rhs_str);

    return def_str;

}

// set statement : int a = 12
// set("a" 12)
fn generate_set_str(sexp_tree : &trees::Node<&str>) -> String{
    let mut iter = sexp_tree.iter();

    let var = iter.next().unwrap();
    let var_str = convert_c_sexp_to_c(var);

    let rhs = iter.next().unwrap();
    let rhs_str = convert_c_sexp_to_c(rhs);

    let def_str = format!("{} = {};\n" , var_str, rhs_str);

    return def_str;

}

// if statement : if(cond){then}{else}
// IF(OP(< 1 2) 2 3)
fn generate_if_str(sexp_tree : &trees::Node<&str>) -> String{
    let mut iter = sexp_tree.iter();

    let cond = iter.next().unwrap();
    let cond_str = convert_c_sexp_to_c(cond);

    let then = iter.next().unwrap();
    let then_str = convert_c_sexp_to_c(then);

    let _else = iter.next().unwrap();
    let else_str = convert_c_sexp_to_c(_else);

    let def_str = format!("if({})\n{{\n{}}}else{{\n{}}}\n" , cond_str, then_str, else_str);

    return def_str;

}

// arg_pair : a : int
// ARG_PAIR(a int)
fn generate_arg_pair_str(sexp_tree : &trees::Node<&str>) -> String{
    let mut iter = sexp_tree.iter();

    let _type = iter.next().unwrap();
    let type_str = convert_c_sexp_to_c(_type);

    let var = iter.next().unwrap();
    let var_str = convert_c_sexp_to_c(var);

    let pair_str = format!("{} {}, " , type_str, var_str);

    return pair_str;

}

// struct : a : int
// FUNC("int" "foo" BODY (ARG_PAIR char x) (ARG_PAIR int y)...)
// note the BODY is inside the center
// int foo (char x, int y, ...){body};
fn generate_func_str(sexp_tree : &trees::Node<&str>) -> String{
    let mut iter = sexp_tree.iter();

    let ret_type = iter.next().unwrap();
    let ret_type_str = convert_c_sexp_to_c(ret_type);   
    
    let name = iter.next().unwrap();
    let name_str = convert_c_sexp_to_c(name);

    let body = iter.next().unwrap();
    let body_str = convert_c_sexp_to_c(body);


    let mut pairs_str = "".to_string();
    let mut arg_pair = iter.next();

    while arg_pair != None{
        let pair_str = convert_c_sexp_to_c(arg_pair.unwrap());
        pairs_str = format!("{} {}", pairs_str, pair_str);
        arg_pair = iter.next();

    }

    let result = format!("{} {} ({}){{\n{}\n}};" , ret_type_str, name_str, pairs_str, body_str);

    return result;

}

// struct_pair : a : int
// STRUCT_PAIR(a int)
fn generate_struct_pair_str(sexp_tree : &trees::Node<&str>) -> String{
    let mut iter = sexp_tree.iter();

    let _type = iter.next().unwrap();
    let type_str = convert_c_sexp_to_c(_type);

    let var = iter.next().unwrap();
    let var_str = convert_c_sexp_to_c(var);

    let pair_str = format!("{} {};\n" , type_str, var_str);

    return pair_str;

}

// struct : a : int
// STRUCT("foo" "a"  STRUCT_PAIR(a int)+)
// -> typedef struct {char *name;} foo;
fn generate_struct_str(sexp_tree : &trees::Node<&str>) -> String{
    let mut iter = sexp_tree.iter();

    let name = iter.next().unwrap();
    let name_str = convert_c_sexp_to_c(name);

    let mut pairs_str = "".to_string();

    let mut pair = iter.next();

    while pair != None{
        let pair_str = convert_c_sexp_to_c(pair.unwrap());
        pairs_str = format!("{} {}", pairs_str, pair_str);
        pair = iter.next();

    }

    let result = format!("typedef struct{{\n{}}} {};\n" , pairs_str, name_str);

    return result;

}




// binary operator (eg. (+ 2 3))
fn generate_op_str(sexp_tree : &trees::Node<&str>) -> String{
    let mut iter = sexp_tree.iter();
    let head = iter.next();
    let operator = head.unwrap().data();

    let lhs =  iter.next().unwrap();
    let rhs = iter.next().unwrap();


    let mut lhs_str = convert_c_sexp_to_c(lhs);
    let mut rhs_str = convert_c_sexp_to_c(rhs);


    let op_str = format!("({} {} {})", lhs_str , operator, rhs_str);
    
    return op_str;
}

fn convert_c_sexp_to_c(sexp_tree : & trees::Node<&str>) -> String{

    let root_data = *(sexp_tree.data());

    if root_data == "EXEC"{
        return generate_exec_str(sexp_tree);

    }else if root_data == "IF"{
        return generate_if_str(sexp_tree);
    }
    else if root_data == "INCL"{
        return generate_incl_str(sexp_tree);
    }else if root_data == "DEF"{
        return generate_def_str(sexp_tree);
    }else if root_data == "ARG_PAIR"{
        return generate_arg_pair_str(sexp_tree);
    }else if root_data == "FUNC"{
        return generate_func_str(sexp_tree);
    }else if root_data == "STRUCT_PAIR"{
        return generate_struct_pair_str(sexp_tree);
    }else if root_data == "STRUCT"{
        return generate_struct_str(sexp_tree);
    }else if root_data == "SET"{
        return generate_set_str(sexp_tree);
    }else if root_data == "STMTS"{
        return generate_stmts_str(sexp_tree);
    }
    else if root_data == "OP"{
        return generate_op_str(sexp_tree);
    }

    else {
        
        return sexp_tree.data().to_string();}
}



pub fn main() -> std::io::Result<()>{

    let mut c_like_expr_trees = tr("STMTS")/(tr("INCL") /tr("stdio.h"))/(tr("EXEC")/(tr("printf") -tr("\"strng=%s\"") -tr("strng")))/(tr("DEF")/tr("int")/tr("a")/(tr("OP")/tr("+")/(tr("OP")/tr("*")/tr("2")/tr("3"))/tr("8")));

    println!("{:?}", c_like_expr_trees.to_string());


    let res_str =  convert_c_sexp_to_c(c_like_expr_trees.root());


    let mut file = File::create("/tmp/foo.txt")?;
    file.write_all(res_str.as_bytes())?;
    Ok(())


}




#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;

    #[test]
    fn c_sexp() -> std::io::Result<()>{
        // Test 1
        let c_like_expr_trees = tr("STMTS")/
                                    (tr("INCL") /tr("stdio.h"))
                                    /(tr("EXEC")/(tr("printf") -tr("\"strng=%s\"") -tr("strng")))
                                    /(tr("DEF")/tr("int")/tr("a")
                                        /(tr("OP")/tr("+")
                                            /(tr("OP")/tr("*")/tr("2")/tr("3"))
                                            /tr("8")));


        println!("{:?}", c_like_expr_trees.to_string());

        let res_str =  convert_c_sexp_to_c(c_like_expr_trees.root());

        let actual = "#include<stdio.h>
;
printf(\"strng=%s\",strng);
int a = ((2 * 3) + 8);
;
";
        assert_eq!(actual, res_str);


        // Test 2
        let c_like_expr_trees2 = tr("STMTS")/(tr("SET")/tr("int")/tr("a")/tr("8"));

        println!("{:?}", c_like_expr_trees2.to_string());

        let res_str2 =  convert_c_sexp_to_c(c_like_expr_trees2.root());


        let verifiter2 = "int = a;
;
";
        assert_eq!(res_str2, verifiter2);

        // Test 3
        let c_like_expr_trees3 = tr("STMTS")/(tr("IF")/(tr("OP")/tr("==")/tr("a")/tr("8"))/tr("a")/tr("false"));

        println!("{:?}", c_like_expr_trees3.to_string());

        let res_str3 =  convert_c_sexp_to_c(c_like_expr_trees3.root());

        let mut file = File::create("/tmp/foo.txt")?;
        file.write_all(res_str3.to_string().as_bytes())?;

        let verifiter3 = "if((a == 8))
{
a}else{
false}
;
";

        assert_eq!(res_str3, verifiter3);




        // Test 4
        let c_like_expr_trees4_tuple = ("STRUCT", "stt", ("STRUCT_PAIR", "int", "name"), ("STRUCT_PAIR" ,"double", "pour"));
        
        let c_like_expr_trees4 =trees::Tree::<&str>::from_tuple(c_like_expr_trees4_tuple);

        let c_like_expr_trees4_str = convert_c_sexp_to_c(c_like_expr_trees4.root());

        assert_eq!("typedef struct{
 int name;
 double pour;
} stt;
", c_like_expr_trees4_str); 

        // Test 5
        let c_like_expr_trees5 = tr("FUNC")/tr("int")/tr("stt")/(tr("STMTS")/(tr("OP")/tr("*")/tr("12")/tr("14")))/(tr("ARG_PAIR")/tr("int")/tr("name"))/(tr("ARG_PAIR")/tr("double")/tr("pour"));

        
        println!("{:?}", c_like_expr_trees5.to_string());

        let c_like_expr_trees5_str = convert_c_sexp_to_c(c_like_expr_trees5.root());

        assert_eq!("int stt ( int name,  double pour, ){
(12 * 14);

};", c_like_expr_trees5_str);

 


        Ok(())

    }






}