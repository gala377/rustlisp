use std::any::TypeId;

use crate::{eval::Interpreter, gc::{Heap, Root, Set, Weak}, runtime::RootedVal};



pub type DynDispatchFn = fn(Root<NativeStructPointer>, &mut Interpreter, &str, Vec<RootedVal>) -> RootedVal;


pub struct NativeStructVTable {
    pub dispatch: DynDispatchFn,
}


pub trait NativeStruct: Sized {
    fn dispatch(this: Root<NativeStructPointer>, vm: &mut Interpreter, method: &str, args: Vec<RootedVal>) -> RootedVal;

    fn vtable() -> &'static NativeStructVTable;
}

pub trait NativeErased {
    fn visit(&self, _marked: &mut Set<usize>, _heap: &mut Heap) {
        // blank implementation by default
    }

    fn type_id(&self) -> TypeId;
}

#[macro_export]
macro_rules! define_native_struct {
    ($name:ty where vtable = $vtable:ident {
        $dispatch:item
     }) => {

        static $vtable: NativeStructVTable = NativeStructVTable {
            dispatch: <$name as NativeStruct>::dispatch,
        };

        impl NativeErased for $name {
            fn type_id(&self) -> TypeId {
                TypeId::of::<$name>()
            }
        }
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

// TODO make those type safe by 

pub fn to_self_mut<'a, T: 'static>(this: &'a mut NativeStructPointer) -> Option<&'a mut T> {
    cast_to_self_mut(&mut this.data)
}

pub fn to_self<'a, T: 'static>(this: &'a NativeStructPointer) -> Option<&'a T> {
    cast_to_self(&this.data)
}

fn cast_to_self_mut<'a, T: 'static>(this: &'a mut Box<dyn NativeErased>) -> Option<&'a mut T> {
    let reference: &mut dyn NativeErased = &mut **this;
    if reference.type_id() == TypeId::of::<T>() {
        Some(unsafe { &mut *(reference as *mut dyn NativeErased as *mut T) })
    } else {
        None
    }
}

fn cast_to_self<'a, T: 'static>(this: &'a Box<dyn NativeErased>) -> Option<&'a T> {
    let reference: &dyn NativeErased = &**this;
    if reference.type_id() == TypeId::of::<T>() {
        Some(unsafe { &*(reference as *const dyn NativeErased as *const T) })
    } else {
        None
    }
}

pub struct NativeStructPointer {
    pub data: Box<dyn NativeErased>,
    pub vtable: &'static NativeStructVTable,
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

pub fn with_self<SelfType, Func, Res>(ptr: &Root<NativeStructPointer>, vm: &Interpreter, func: Func) -> Option<Res> 
where
    SelfType: 'static,
    Func: FnOnce(&SelfType, &Interpreter) -> Res,
{
    let self_ref = vm.get_ref(ptr);
    let self_: &SelfType = to_self(&*self_ref)?;
    Some(func(self_, vm))
}

pub fn with_self_mut<SelfType, Func, Res>(ptr: &mut Root<NativeStructPointer>, vm: &mut Interpreter, func: Func) -> Option<Res> 
where
    SelfType: 'static,
    Func: FnOnce(&mut SelfType) -> Res,
{
    let mut self_ref = vm.get_mut(ptr);
    let self_: &mut SelfType = to_self_mut(&mut *self_ref)?;
    Some(func(self_))
}
#[cfg(test)]
mod tests {

    use super::*;
    struct Foo { a: f64, b: f64 }

    define_native_struct!{
        Foo where vtable = FOO_VTABLE {
            fn dispatch(mut this: Root<NativeStructPointer>, vm: &mut Interpreter, method: &str, _args: Vec<RootedVal>) -> RootedVal {
                match method {
                    "plus" => {
                        let res = with_self_mut(&mut this, vm, |self_: &mut Self| {
                            self_.a += 10.0;
                            self_.a + self_.b
                        }).unwrap();
                        return RootedVal::NumberVal(res);
                    }
                    _ => panic!("Wrong method")
                }
            }
        }
    }

    #[test]
    fn native_struct_pointer_can_be_created() {
        let foo = Foo{a: 1.0, b: 2.0};
        let _ptr = NativeStructPointer::new(foo);
    }


}