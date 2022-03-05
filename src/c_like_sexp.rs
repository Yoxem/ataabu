use trees::tr;
use std::fs::File;
use std::io::prelude::*;


fn generate_exec_str(sexp_tree : trees::Node<&str>) -> String{
    let mut iter = sexp_tree.iter();
    let mut result : String = "".to_string();
    let mut args_str : String = "".to_string();


    let func_str = iter.next().unwrap();


    let sexp_tree_degree = sexp_tree.degree();

    let mut next_subitem = iter.next();

    for _i in 0..(sexp_tree_degree - 1){
        args_str = format!("{},{}", args_str, next_subitem.unwrap().data());
        next_subitem = iter.next();
    }

    args_str = args_str[1..].to_string(); // remove the foremost ','.

    result = format!("{}({});\n", func_str , args_str);

    return result;
}

fn generate_incl_str(sexp_tree : trees::Node<&str>) -> String{

    let mut iter = sexp_tree.iter();

    let importee_str = iter.next().unwrap();

    println!("==============={:?}", importee_str);

    let result : String = format!("#include<{}>\n", importee_str);

    return result;

}

fn generate_stmts_str(sexp_tree : trees::Node<&str>) -> String{

    let mut iter = sexp_tree.iter();
    let mut stmts_str : String = "".to_string();


    let sexp_tree_degree = sexp_tree.degree();

    let mut next_subitem = iter.next();

    for _i in 0..(sexp_tree_degree){
        stmts_str = format!("{},{}", stmts_str, convert_c_sexp_to_c(*(next_subitem.unwrap()));
        next_subitem = iter.next();
    }

    stmts_str = stmts_str[1..].to_string(); // remove the foremost ','.


    return stmts_str;

}

fn convert_c_sexp_to_c(sexp_tree : trees::Node<&str>) -> String{

    let root_data = *(sexp_tree.root().data());

    if root_data == "EXEC"{
        return generate_exec_str(sexp_tree);

    }else if root_data == "INCL"{
        return generate_incl_str(sexp_tree);
    }else if root_data == "STMTS"{
        return generate_stmts_str(sexp_tree);
    }

    else {return "".to_string();}
}



pub fn main() -> std::io::Result<()>{
    let mut c_like_expr_trees = tr("STMTS")/(tr("INCL") /tr("stdio.h"))/(tr("EXEC")/(tr("printf") -tr("\"strng=%s\"") -tr("strng")));

    println!("{:?}", c_like_expr_trees.to_string());
    // let mut c_like_expr_trees = tr("EXEC") /(tr("printf") -tr("\"strng=%s\"") -tr("strng"));


    let res_str =  convert_c_sexp_to_c(c_like_expr_trees);


    let mut file = File::create("/tmp/foo.txt")?;
    file.write_all(res_str.as_bytes())?;
    Ok(())


}