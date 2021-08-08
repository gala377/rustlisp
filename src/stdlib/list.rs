use crate::{native_module, runtime::RootedVal};

use crate::runtime::RootedVal::*;

native_module! {
    make_list(vm, args) => RootedVal::list_from_rooted(args, &mut vm.heap);

    is_list(vm, args) {
        assert_eq!(args.len(), 1, "list? takes one argument");
        if let List(_) = &args[0] {
            RootedVal::sym_true()
        } else {
            RootedVal::sym_false()
        }
    };

    typed is_empty(vm, List(val)) =>
        RootedVal::predicate(vm.heap.deref_ptr(val).is_empty());

    typed get_nth_elem(vm, List(val), NumberVal(index)) {
        let index = *index as usize;
        vm.heap.deref_ptr(val)[index].as_root()
    };

    typed get_len(vm, List(val)) =>
        RootedVal::NumberVal(vm.heap.deref_ptr(val).len() as f64);

    typed map(vm, func, List(val)) {
        if !func.is_callable() {
            panic!("First argument of map needs to be a callable");
        }
        let len = vm.heap.deref_ptr(val).len();
        let mut res = Vec::with_capacity(len);
        for i in 0..len {
            let item = vm.heap.deref_ptr(val)[i].as_root();
            res.push(vm.call(func, vec![item]));
        }
        RootedVal::list_from_rooted(res, &mut vm.heap)
    };

    typed push(vm, List(list), val) {
        let val = val.as_weak();
        vm.heap.deref_ptr_mut(list).push(val);
        RootedVal::none()
    };

    typed head(vm, List(val)) {
        assert!(!vm.heap.deref_ptr(val).is_empty(), "Cannot head on empty list");
        vm.heap.deref_ptr(val)[0].as_root()
    };

    typed size(vm, List(val)) =>
        RootedVal::NumberVal(vm.heap.deref_ptr(val).len() as f64);

}
