use std::cell::UnsafeCell;

use crate::{eval::Interpreter, gc::{self, Allocable, Heap, Root, Set, Weak}, runtime::RootedVal};

pub type DynDispatchFn = fn(Root<()>, &mut Interpreter, &str, Vec<RootedVal>) -> RootedVal;
pub type DynVisitFn = fn(*const (), &gc::MarkSweep, &mut Set<usize>, &mut Heap);
pub type DynDropFn = fn(*mut UnsafeCell<()>);

pub struct VirtualTable {
    pub dispatch: DynDispatchFn,
    pub visit: DynVisitFn,
    pub drop: DynDropFn,
}

pub trait Dispatch: Sized {
    fn dispatch(this: &mut Root<Self>, vm: &mut Interpreter, method: &str, args: Vec<RootedVal>) -> RootedVal;
    fn visit(&self, _gc: &gc::MarkSweep, _marked: &mut Set<usize>, _heap: &mut Heap) {}
}


pub unsafe trait NativeStruct: Dispatch {
    fn erased_dispatch(this: Root<()>, vm: &mut Interpreter, method: &str, args: Vec<RootedVal>) -> RootedVal;
    fn erased_visit(this: *const (), gc: &gc::MarkSweep, marked: &mut Set<usize>, heap: &mut Heap);
    fn erased_drop(this: *mut UnsafeCell<()>);
    fn vptr() -> &'static VirtualTable;
}

#[macro_export]
macro_rules! dispatchable_import_path {
    ( [ $($import:path),* ] $name:ident ) => {
        $(use $import;)*
    };
    ( $name:ident ) => { use super::super::$name; };
}

#[macro_export]
macro_rules! register_native_type{
    ($($name:ident $( [ $($import:path),* ] )?),+) => {
        mod vtables {
            $(
                #[allow(non_snake_case)]
                pub mod $name {
                    use $crate::native::{VirtualTable, NativeStruct};
                    crate::dispatchable_import_path!($( [ $($import),* ] )? $name);
                    pub static VIRTUAL_TABLE: VirtualTable = VirtualTable {
                        dispatch: <$name as NativeStruct>::erased_dispatch,
                        visit: <$name as NativeStruct>::erased_visit,
                        drop: <$name as NativeStruct>::erased_drop,
                    };
                }
            )+
        }

        $(
        unsafe impl $crate::native::NativeStruct for $name {
            fn erased_dispatch(
                this: $crate::gc::Root<()>,
                vm: &mut $crate::eval::Interpreter,
                method: &str,
                args: Vec<$crate::runtime::RootedVal>
            ) -> $crate::runtime::RootedVal {
                let mut self_ = unsafe { this.cast::<$name>() } ;
                let res = <$name as $crate::native::Dispatch>::dispatch(
                    &mut self_,
                    vm,
                    method,
                    args);
                vm.heap.drop_root(self_);
                res
            }

            fn erased_visit(
                this: *const (),
                gc: & $crate::gc::MarkSweep,
                marked: &mut $crate::gc::Set<usize>,
                heap: &mut $crate::gc::Heap)
            {
                <$name as $crate::native::Dispatch>::visit(
                    unsafe { &*(this as *const $name) },
                    gc,
                    marked,
                    heap,
                );
            }

            fn erased_drop(this: *mut UnsafeCell<()>) {
                drop(unsafe { Box::from_raw(this as *mut std::cell::UnsafeCell<$name>) });
            }

            fn vptr() -> &'static $crate::native::VirtualTable {
                &vtables::$name::VIRTUAL_TABLE
            }

        }

        impl $crate::gc::Allocable for $name {
            fn tag() -> $crate::gc::TypeTag {
                $crate::gc::TypeTag::UserType
            }
        }
        )+
    };
}

pub struct RootedStructPtr {
    pub data: Root<()>,
    pub vptr: &'static VirtualTable,
}

impl RootedStructPtr {
    pub fn new<T: NativeStruct + Allocable>(native_struct: T, heap: &mut Heap) -> Self {
        let data = unsafe { heap.allocate_user_type(native_struct).cast::<()>() };
        Self {
            data,
            vptr: T::vptr(),
        }
    }

    pub fn downgrade(self, heap: &mut Heap) -> WeakStructPtr {
        WeakStructPtr {
            data: heap.downgrade(self.data),
            vptr: self.vptr,
        }
    }

    pub fn dispatch(&self, vm: &mut Interpreter, method: &str, args: Vec<RootedVal>) -> RootedVal {
        let self_ = vm.heap.clone_root(&self.data);
        (self.vptr.dispatch)(self_, vm, method, args)
    }

    pub fn heap_drop(self, heap: &mut Heap) {
        heap.drop_root(self.data)
    }

    pub fn clone(&self, heap: &mut Heap) -> Self {
        Self {
            data: heap.clone_root(&self.data),
            vptr: self.vptr,
        }
    }
}

#[derive(Clone)]
pub struct WeakStructPtr {
    pub data: Weak<()>,
    pub vptr: &'static VirtualTable
}

impl WeakStructPtr {
    pub fn upgrade(self, heap: &mut Heap) -> RootedStructPtr {
        RootedStructPtr {
            data: heap.upgrade(self.data),
            vptr: self.vptr,
        }
    }

    pub fn dispatch(&self, vm: &mut Interpreter, method: &str, args: Vec<RootedVal>) -> RootedVal {
        let self_ = vm.heap.upgrade(self.data.clone());
        (self.vptr.dispatch)(self_, vm, method, args)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    struct Foo(usize);
    impl Dispatch for Foo {
        fn dispatch(this: &mut Root<Foo>, vm: &mut Interpreter, _method: &str, _args: Vec<RootedVal>) -> RootedVal {
            let val = vm.get_ref(this).0;
            RootedVal::NumberVal(val as f64)
        }
    }

    crate::register_native_type!(Foo);
}