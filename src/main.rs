extern crate unicode_segmentation;
mod c_like_sexp;

use unicode_segmentation::UnicodeSegmentation;

///
/// Storing the processed string
/// # Items
/// `is_succeed ` : check if it's passed to the parser-pass.
/// `matched` : the current accumulated matched string (in the progress of tokenizing).
/// `remained` : the remained surplus string to be tokenized.
#[derive(Debug, Clone)]
struct ProcessedString {
    is_succeed: bool,
    matched: String,
    remained: String,
}

/// Token-storing struct.
/// # Items
/// * `token_str` = `12`, `2.2`,`"123"`, `_a99`, etc.
/// * `token_type` = `BOOL`, `INT`, `FLO`, `STR`, `ID`, `OP`. etc.
/// * `line_no`: the 1st character's line_no started from 1
///* ` col_no` : the 1st character's line_no started from 1
#[derive(Debug)]
struct Token{
    token_str: String,
    token_type: String,
    line_no: u64,
    col_no: u64,
}

/// The struct storing current current whole line number and column number
/// i.e. a position for a token
/// # Items
/// * `line_no`: the 1st character's line_no started from 1
///* ` col_no` : the 1st character's line_no started from 1
#[derive(Debug)]
struct WholePositionToken{
    line_no: u64,
    col_no: u64,
}


/// get_float token from a input string and return the float token and 
/// return a pair of (maybe<Token>, string)
/// maybe<Token> that may be nothing or containing the result
/// string is the remained token, typed `"FLO"`.
/// 
/// ``` float = [0-9]+[.][0-9]+```
/// # arguments
/// * `input_str` - the string to be tokenized
/// # return value
/// * `(Option<Token>, String)`
///  * the 1st `Option<Token>` : the result whose `token.tyle="FLO"` or `None`
///  * the second `String` : the ramained surplus string
///
/// 
/// ```
/// 
///    let e = get_float("12.345 123454 as ".to_string(), &mut whole_token);
///     // Some(Token { token_str: "12.345", token_type: "FLO", line_no: 1, col_no: 1 }) ,  123454 as
/// 
/// println!("{:?} , {}", e.0, e.1); 
///    let f = get_float("123454 as ".to_string(), &mut whole_token)
///     //None , 123454 as
///     println!("{:?} , {}", f.0, f.1); 
///  ```
/// 
fn get_float(input_str: String, whole_token: &mut WholePositionToken)->(Option<Token>, String){
    let mut is_totally_succeed = false;
    let mut passed1 = false;
    let orig_input_str = input_str.clone();
    
    let mut input = ProcessedString {
        is_succeed: true,
        matched: "".to_string(),
        remained:input_str,
    };
    
    while input.is_succeed == true{
        input = match_range("0".to_string(), "9".to_string(),input);
        passed1 = true;
    }
    
    
    if passed1 == true{
        input.is_succeed = true;
    }
    
    let input_clone = input.clone();
    

    let new_input = match_range(".".to_string(), ".".to_string(),input_clone);
    
    if new_input.is_succeed == true
        {
            input = new_input.clone();

        }
    
     while input.is_succeed == true{
        input = match_range("0".to_string(), "9".to_string(),input);
        if input.is_succeed == true {
            is_totally_succeed = true;
        }
    }
    
    
    if is_totally_succeed == false{
        return (None, orig_input_str);
    }
    else{

        let matched_string = input.matched;
        let token_length = UnicodeSegmentation::graphemes(matched_string.as_str(), true).count();
    
        let token_returned = Token{
            token_str: matched_string,
            token_type: "FLO".to_string(),
            line_no : whole_token.line_no,
            col_no : whole_token.col_no,
        };

        let total_returned =  (Some(token_returned), input.remained);

        whole_token.col_no += token_length as u64;

        return total_returned;
        
    }

}



/// get_integer token from a input string and return the integer token and 
/// return a pair of (maybe<Token>, string)
/// maybe<Token> that may be nothing or containing the result
/// string is the remained token, typed `"INT"`.
/// # arguments
/// * `input_str` - the string to be tokenized
/// # return value
/// * `(Option<Token>, String)`
///  * the 1st `Option<Token>` : the the result whose `token.tyle="INT" or `None`
///  * the second `String` : the ramained surplus string
/// // integer = [0-9]+
///
/// eg.
///  ```
////  let c = get_integer("as 123454 as ".to_string());
///    println!("{:?} , {}", c.0, c.1);
///    // return None , as 123454 as 
///    
///    let d = get_integer("123454 as ".to_string());
///    println!("{:?} , {}", d.0, d.1);
///    // return Some(Token { token_str: "123454", token_type: "INT" }) ,  as
///   ```
///
fn get_integer(input_str: String, whole_token: &mut WholePositionToken)->(Option<Token>, String){
    let mut input = ProcessedString {
        is_succeed: true,
        matched: "".to_string(),
        remained:input_str,
    };
    
    while input.is_succeed == true{
        input = match_range("0".to_string(), "9".to_string(),input);
    }
    
    if input.matched == ""{
        return (None, input.remained);
    }
    else{
    
        let matched_string = input.matched;
        let token_length = UnicodeSegmentation::graphemes(matched_string.as_str(), true).count();
    
        let token_returned = Token{
            token_str: matched_string,
            token_type: "INT".to_string(),
            line_no : whole_token.line_no,
            col_no : whole_token.col_no,
        };


        let total_returned =  (Some(token_returned), input.remained);

        whole_token.col_no += token_length as u64;

        return total_returned;
        
        
    }

}


/// input the char as a `String` with min index, and the char as a `String` with max index, and the string
/// processed by the lexer wrapped as `ProcessedString`, then return the value is:
/// if it successed to be processed, the 1st char in remained is moved to the
/// tail of the string matched. otherwise, the successed indicator
/// 'is_succeed' is set to False. eg.
/// ```
///    let a =  ProcessedString {
///    is_succeed: true,
///    matched: "".to_string(),
///    remained: "c59".to_string()};
///    
///    let b =  ProcessedString {
///    is_succeed: true,
///    matched: "".to_string(),
///    remained: "159".to_string()};
///    
///    let a_res =  match_range("a".to_string(), "z".to_string(), a); // matched
///    let b_res =  match_range("a".to_string(), "z".to_string(), b); // unmatched
///    
///    println!("succeed: {}, char: {}", a_res.is_succeed, a_res.matched);
///    println!("succeed: {}, char: {}", b_res.is_succeed, b_res.matched);
/// 
/// // returns:
/// succeed: true, matched: c
/// succeed: false, matched: 
/// ```
/// # arguments
/// * `char_min` : the character as the lower bound of index of a range of a character
/// * `char_max`: the character as the upper bound of index of a range of a character
/// * `maybe_string`: the container of the processed string.
/// # return value
/// * `ProcessedString`:
/// * matched nothing => `is_succeed` attr. is `false`
/// * match a character => `is_succeed` is `true`.

fn match_range(
    char_min: String,
    char_max: String,
    mut maybe_string: ProcessedString,
) -> ProcessedString {
    if maybe_string.remained.len() == 0 {
        maybe_string.is_succeed = false;

        return maybe_string;
    } else if maybe_string.is_succeed == false {
        return maybe_string;
    } else {
        let first_char_index = maybe_string.remained.chars().nth(0).unwrap() as i64;

        let char_max_index = char_max.chars().nth(0).unwrap() as i64;
        let char_min_index = char_min.chars().nth(0).unwrap() as i64;

        if (first_char_index >= char_min_index) & (first_char_index <= char_max_index){
            let result = format!(
                "{}{}",
                &maybe_string.matched[0..],
                &maybe_string.remained[0..1]
            );
            let surplus = &maybe_string.remained[1..];

            let output = ProcessedString {
                is_succeed: true,
                matched: result,
                remained: surplus.to_string(),
            };

            return output;
        } else {
            maybe_string.is_succeed = false;

            return maybe_string;
        }
    }
}

fn main() {

    let mut whole_token = WholePositionToken{
        line_no : 1,
        col_no  : 1,
    };

    
    let e = get_float("12.345 123454 as ".to_string(), &mut whole_token);
    // Some(Token { token_str: "12.345", token_type: "FLO", line_no: 1, col_no: 1 }) ,  123454 as
    println!("{:?} , {}", e.0, e.1); 


    let f = get_float("123454 as ".to_string(), &mut whole_token);
    //None , 123454 as 
    println!("{:?} , {}", f.0, f.1); 

    c_like_sexp::main();

}
