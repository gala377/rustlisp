use std::env;

use lispylib::{data, gc};
use lispylib::runtime;
use lispylib;

fn get_file_name(args: Vec<String>) -> String {
    match args.len() {
        1 => "input.rlp".to_string(),
        2 => args[1].clone(),
        _ => panic!("Provide one argument - file to interpret"),
    }
}

fn main() -> Result<(), lispylib::ParseError> {
    let filename = get_file_name(env::args().collect());
    let source_code = std::fs::read_to_string(filename).unwrap();
    let lispylib::AST {
        program,
        mut symbol_table_builder,
    } = lispylib::read(&source_code)?;
    print!("\n\n\n");
    let env = lispylib::stdlib::std_env(&mut symbol_table_builder);
    let mut symbol_table = symbol_table_builder.build();
    for expr in &program {
        lispylib::eval(env.clone(), None, &mut symbol_table, expr);
    }
    Ok(())
}

// fn main() {
//     let list = runtime::List{data: Vec::new()};
//     let string = runtime::String{data: "Hello".into()};
//     let mut heap = gc::Heap::new();
//     let mut list = heap.allocate(list);
//     let string = heap.allocate(string);
//     let mut list2 = list.clone();
//     unsafe {
//         list2.data.as_mut().data.push(data::RuntimeVal::NumberVal(1.999));
//     }
//     println!("Does it work? {}", unsafe {
//         match list.data.as_ref().data[0] {
//             data::RuntimeVal::NumberVal(x) => x,
//             _ => panic!("Not working :c"),
//         }
//     });
// }