use crate::{native::Dispatch, native_module, register_native_type, runtime::RootedVal};

struct Int(i64);

impl Dispatch for Int {
    fn dispatch(
        this: &mut crate::gc::Root<Self>,
        vm: &mut crate::eval::Interpreter,
        method: &str,
        args: Vec<crate::runtime::RootedVal>,
    ) -> crate::runtime::RootedVal {
        assert_eq!(args.len(), 0, "Args for int take no arguments");
        match method {
            "inc" => vm.get_mut(this).0 += 1,
            "dev" => vm.get_mut(this).0 -= 1,
            "get" => return RootedVal::NumberVal(vm.get_ref(this).0 as f64),
            _ => panic!("Unknown method {}", method),
        };
        RootedVal::none()
    }
}

register_native_type!(Int);

use RootedVal::*;

native_module! {
    typed new_counter(vm, NumberVal(start)) {
        RootedVal::user_type(Int(*start as i64), &mut vm.heap)
    };
}
