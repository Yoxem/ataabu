#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(feature = "std"))]
use core::str;
use combine::lib::println;

#[cfg(feature = "std")]
use std::str;

use combine::{
    error::UnexpectedParse,
    parser::{
        char::digit,
        choice::{choice, optional},
        range::recognize,
        repeat::{skip_many, skip_many1},
        token::token,
        combinator::attempt
    },
    Parser,
    lib::vec::Vec,
};

fn main() {

    let remained = "+";
    let mut vec : Vec<&str> = Vec::new();

    //while remained != ""{


        let mut parser = choice((
            optional( choice((token('*'),token('/')))),
             optional( choice((token('+'),token('-')))),));
        let result = parser.parse(remained);


        println!("{:?}", result);



    //}
}