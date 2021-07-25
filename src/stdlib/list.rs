use crate::{
    native_module,
    runtime::{drop_rooted_vec, RootedVal},
};

use crate::runtime::RootedVal::*;

native_module!{
    make_list(vm, args) => RootedVal::list_from_rooted(args, &mut vm.heap);

    is_list(vm, args) {
        assert_eq!(args.len(), 1, "list? takes one argument");
        let res = if let List(_) = &args[0] {
            RootedVal::sym_true()
        } else {
            RootedVal::sym_false()
        };
        drop_rooted_vec(&mut vm.heap, args);
        res
    };

    typed is_empty(vm, List(val)) =>
        RootedVal::predicate(vm.heap.deref_ptr(val).is_empty());

    typed get_nth_elem(vm, List(val), NumberVal(index)) {
        let index = *index as usize;
        let elem = vm.heap.deref_ptr(val)[index].clone();
        elem.upgrade(&mut vm.heap)
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
            let item = vm.heap.deref_ptr(val)[i].clone();
            let item = item.upgrade(&mut vm.heap);
            res.push(vm.call(func, vec![item]));
        }
        RootedVal::list_from_rooted(res, &mut vm.heap)
    };
}
