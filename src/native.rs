use crate::{eval::Interpreter, gc::{Heap, Root, Set, Weak}, runtime::RootedVal};



pub type DynDispatchFn = fn(Root<dyn NativeErased>, &mut Interpreter, &str, Vec<RootedVal>) -> RootedVal;


pub struct NativeStructVTable {
    pub dispatch: DynDispatchFn,
}


pub trait NativeStruct: Sized {
    fn dispatch(this: &mut Root<Self>, vm: &mut Interpreter, method: &str, args: Vec<RootedVal>) -> RootedVal;

    fn cast_and_dispatch(this: Root<dyn NativeErased>, vm: &mut Interpreter, method: &str, args: Vec<RootedVal>) -> RootedVal {
        let mut casted = unsafe { vm.heap.clone_root(&this).cast::<Self>() } ;
        let res = Self::dispatch(&mut casted, vm, method, args);
        vm.heap.drop_root(this);
        vm.heap.drop_root(casted);
        res
    }

    fn vtable() -> &'static NativeStructVTable;
}

pub trait NativeErased {
    fn visit(&self, _marked: &mut Set<usize>, _heap: &mut Heap, _entry_index: usize) {
        // blank implementation by default
    }
}

#[macro_export]
macro_rules! define_native_struct {
    ($name:ty where vtable = $vtable:ident {
        $dispatch:item
     }) => {

        static $vtable: NativeStructVTable = NativeStructVTable {
            dispatch: <$name as NativeStruct>::cast_and_dispatch,
        };

        impl NativeErased for $name {}
        impl NativeStruct for $name {
            $dispatch

            fn vtable() -> &'static NativeStructVTable {
                & $vtable
            }
        }
    };
}

#[macro_export]
macro_rules! define_visitable_native_struct {
    ($name:ty where vtable = $vtable:ident {
        $dispatch:item
        $visit:item
     }) => {

        static $vtable: NativeStructVTable = NativeStructVTable {
            dispatch: <$name as NativeStruct>::dispatch,
        }

        impl NativeErased for $name {
            $visit
        }
        impl NativeStruct for $name {
            $dispatch

            fn vtable() -> NativeStructVTable {
                & $vtable
            }
        }
    };
}

pub struct NativeStructPointer {
    data: Box<dyn NativeErased>,
    vtable: &'static NativeStructVTable,
}

impl NativeStructPointer {
    pub fn new<T: 'static>(val: T) -> Self
    where
        T: NativeErased + NativeStruct
    {
        Self {
            data: Box::new(val),
            vtable: <T as NativeStruct>::vtable(),
        }
    }
}

// #[cfg(test)]
// mod tests {

//     use super::*;
//     struct Foo { a: f64, b: f64 }

//     define_native_struct!{
//         Foo where vtable = FOO_VTABLE {
//             fn dispatch(this: &mut Root<Foo>, vm: &mut Interpreter, method: &str, _args: Vec<RootedVal>) -> RootedVal {
//                 match method {
//                     "plus" => {
//                         let self_ = vm.get_ref(this);
//                         return RootedVal::NumberVal(self_.a + self_.b);
//                     }
//                     _ => panic!("Wrong method")
//                 }
//             }
//         }
//     }

//     #[test]
//     fn native_struct_pointer_can_be_created() {
//         let foo = Foo{a: 1.0, b: 2.0};
//         let _ptr = NativeStructPointer::new(foo);
//     }


// }