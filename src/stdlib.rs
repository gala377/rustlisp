use std::rc::Rc;

use crate::data::{Environment, RuntimeVal};

macro_rules! def_func {
    ($map:ident, $name:expr, $lambda:expr) => {
        $map.insert($name.into(), RuntimeVal::function($lambda));
    };
    ($map:ident, $name:expr, $func:path) => {
        $map.insert($name.into(), RuntimeVal::function($lambda));
    };
}

pub fn std_env() -> Rc<Environment> {
    let mut env = Environment::new();
    let map = &mut Rc::get_mut(&mut env)
        .expect("std env was already borrowed before returning it")
        .values;
    def_func!(map, "print", |_, args| {
        for arg in args.into_iter() {
            print!("{}", arg.repr());
            print!(" ");
        }
        RuntimeVal::nil()
    });
    def_func!(map, "repr", |_, args| {
        assert!(args.len() == 1);
        RuntimeVal::StringVal(args[0].repr())
    });
    env
}
