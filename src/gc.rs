use std::{
    cell::UnsafeCell,
    collections::HashMap,
    marker::PhantomData,
    ops::{Deref, DerefMut},
    ptr,
};

use crate::{
    env::Environment,
    eval::{FuncFrame, ModuleState},
    runtime::{self, Lambda, RuntimeFunc, WeakVal},
};

#[cfg(feature = "hash_set")]
type Set<T> = std::collections::HashSet<T>;

#[cfg(not(feature = "hash_set"))]
type Set<T> = std::collections::BTreeSet<T>;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum TypeTag {
    Lambda,
    List,
    String,
    RuntimeFunc,
    None,
}

#[macro_export]
macro_rules! check_ptr {
    ($heap:expr, $ptr:expr) => {
        assert!(
            !&$heap.entries[$ptr.entry_index()].header.dropped,
            "Ptr already dropped"
        )
    };
}

pub struct ScopedPtr<'guard, T> {
    pub value: &'guard T,
}

impl<T> Deref for ScopedPtr<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.value
    }
}

pub struct ScopedMutPtr<'guard, T> {
    pub value: &'guard mut T,
}

impl<T> Deref for ScopedMutPtr<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.value
    }
}

impl<T> DerefMut for ScopedMutPtr<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.value
    }
}

pub trait ScopedRef<T> {
    fn scoped_ref<'a>(&'a self, guard: &'a dyn ScopeGuard) -> &'a T;
    fn scoped_ref_mut<'a>(&'a mut self, guard: &'a mut dyn ScopeGuard) -> &'a mut T;
}

pub trait HeapMarked {
    fn entry_index(&self) -> usize;
}

/// ScopeGuard marks types that can be used as
/// a guard that upholds rust's invariants regarding
/// mutability and shared references when dereferencing
/// Weak or Root pointers.
///
/// In other words: mutable dereference of a pointer depends
/// on mutable borrow of a guard. Immutable dereference of
/// a pointer depends on immutable borrow of a guard.
/// This means that all dereferences that share the same guard
/// are guaranteed, by the guards sharing and ownership, to be safe.
///
/// When implementing this trait it is users responsibility to
/// ensure that proper guard instance will be used with each
/// dereference. Not upholding this might result in
/// undefined behavior.
pub unsafe trait ScopeGuard {}

pub struct Root<T: ?Sized> {
    // TODO: Documentation for `NonNull` states that if you created `NonNull`
    // pointer by using `Into` or `From` traits on shared reference &T then
    // mutations on the T using this pointer are UB unless they happen inside
    // `UnsafeCell`. The pointer here is not obtained through shared reference
    // but through `Box::into_raw(Box::new(T))` so maybe we don't need an
    // `UnsafeCell` here. We should check that.
    data: ptr::NonNull<UnsafeCell<T>>,
    pub entry_index: usize,

    _phantom: PhantomData<T>,
}

impl<T> PartialEq for Root<T> {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}

impl<T> Eq for Root<T> {}

impl<T> ScopedRef<T> for Root<T> {
    fn scoped_ref<'a>(&'a self, _guard: &'a dyn ScopeGuard) -> &'a T {
        // This is safe because the only way to call this method is to pass
        // a guard that has the same borrow as this ref.
        // So we cannot borrow mutably if we borrow guard mutably at the same time.
        // The ScopeGuard trait is an unsafe trait only implemented for
        // a Heap struct in this module which cannot be cloned during
        // mutator's execution.
        // If the user implements ScopeGuard themselves it's their responsibility
        // to ensure that the rust invariants will be upheld.
        unsafe { &*self.data.as_ref().get() }
    }

    fn scoped_ref_mut<'a>(&'a mut self, _guard: &'a mut dyn ScopeGuard) -> &'a mut T {
        // This is safe because the only way to call this method is to pass
        // a guard that has the same borrow as this ref.
        // So we cannot borrow mutably if we borrow guard mutably at the same time.
        // The ScopeGuard trait is an unsafe trait only implemented for
        // a Heap struct in this module which cannot be cloned during
        // mutator's execution.
        // If the user implements ScopeGuard themselves it's their responsibility
        // to ensure that the rust invariants will be upheld.
        unsafe { &mut *self.data.as_ref().get() }
    }
}

impl<T> HeapMarked for Root<T> {
    fn entry_index(&self) -> usize {
        self.entry_index
    }
}

impl<T: ?Sized> Drop for Root<T> {
    fn drop(&mut self) {
        panic!("Root should always be manually dropped");
    }
}

pub struct Weak<T: ?Sized> {
    // TODO: Documentation for `NonNull` states that if you created `NonNull`
    // pointer by using `Into` or `From` traits on shared reference &T then
    // mutations on the T using this pointer are UB unless they happen inside
    // `UnsafeCell`. The pointer here is not obtained through shared reference
    // but through `Box::into_raw(Box::new(T))` so maybe we don't need an
    // `UnsafeCell` here. We should check that.
    data: ptr::NonNull<UnsafeCell<T>>,
    entry_index: usize,
}

impl<T> PartialEq for Weak<T> {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}

impl<T> Eq for Weak<T> {}

impl<T> ScopedRef<T> for Weak<T> {
    fn scoped_ref<'a>(&'a self, _guard: &'a dyn ScopeGuard) -> &'a T {
        // TODO: Actually this is unsafe as weak does not extend references
        // lifetime so the pointer can point to freed data.
        // Technically this should never happen as Interpreter keeps all
        // reachable `Weak` pointers alive but the problem happen if the weak
        // pointer is cloned outside. So to make this absolutely safe we should
        // check if the pointer has been dropped, if not then check if the pointer
        // is still the same as it could change and that means that this pointer
        // was freed. If the pointer is valid we should root it. Then we can
        // safely dereference, which is a lot of work tbh.
        //
        // tl;dr: It is safe if dereferenced `Weak` is inside `Interpreter`'s
        // tracked containers.
        unsafe { &*self.data.as_ref().get() }
    }

    fn scoped_ref_mut<'a>(&'a mut self, _guard: &'a mut dyn ScopeGuard) -> &'a mut T {
        // TODO: Actually this is unsafe as weak does not extend references
        // lifetime so the pointer can point to freed data.
        // Technically this should never happen as Interpreter keeps all
        // reachable `Weak` pointers alive but the problem happen if the weak
        // pointer is cloned outside. So to make this absolutely safe we should
        // check if the pointer has been dropped, if not then check if the pointer
        // is still the same as it could change and that means that this pointer
        // was freed. If the pointer is valid we should root it. Then we can
        // safely dereference, which is a lot of work tbh.
        //
        // tl;dr: It is safe if dereferenced `Weak` is inside `Interpreter`'s
        // tracked containers.
        unsafe { &mut *self.data.as_ref().get() }
    }
}

impl<T> HeapMarked for Weak<T> {
    fn entry_index(&self) -> usize {
        self.entry_index
    }
}

impl<T> Clone for Weak<T> {
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
            entry_index: self.entry_index,
        }
    }
}

pub trait Allocable {
    fn tag() -> TypeTag;
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Header {
    pub size: usize,
    pub tag: TypeTag,
    pub marked: bool,
    pub next_node: Option<usize>,
    pub strong_count: usize,
    pub weak_count: usize,
    pub dropped: bool,
}

impl Header {
    pub fn new() -> Self {
        Self {
            size: 0,
            tag: TypeTag::None,
            marked: false,
            next_node: None,
            strong_count: 0,
            weak_count: 0,
            dropped: false,
        }
    }
    pub fn with_next(next: Option<usize>) -> Self {
        Self {
            size: 0,
            tag: TypeTag::None,
            marked: false,
            next_node: next,
            strong_count: 0,
            weak_count: 0,
            dropped: false,
        }
    }
}

pub struct HeapEntry {
    pub data: Option<ptr::NonNull<UnsafeCell<()>>>,
    pub header: Header,
}

impl HeapEntry {
    // Note that this function just drops data under `data` pointer and then frees the memory.
    // What this means is that the pointer after this function points to freed memory.
    //
    // For a safe version look at `drop_data` which sets the data pointer to `None` after freeing it.
    unsafe fn drop_data_unchecked(&mut self) {
        match &mut self.data {
            None => (),
            Some(ptr) => match &self.header.tag {
                TypeTag::Lambda => {
                    // println!("Dropping lambda");
                    safe_drop_and_free::<runtime::Lambda>(ptr.as_ptr());
                }
                TypeTag::List => {
                    // println!("Dropping list");
                    safe_drop_and_free::<Vec<WeakVal>>(ptr.as_ptr());
                }
                TypeTag::RuntimeFunc => {
                    // println!("Dropping function");
                    safe_drop_and_free::<runtime::RuntimeFunc>(ptr.as_ptr());
                }
                TypeTag::String => {
                    // println!("Dropping string");
                    safe_drop_and_free::<String>(ptr.as_ptr());
                }
                TypeTag::None => (),
            },
        }
    }

    pub fn drop_data(&mut self) {
        if let None = self.data {
            panic!("Free of an empty heap entry");
        }
        debug_assert!(self.header.strong_count == 0);
        unsafe {
            self.drop_data_unchecked();
        }
        self.data = None;
    }
}

unsafe fn safe_drop_and_free<T>(ptr: *mut UnsafeCell<()>) {
    let ptr = ptr as *mut UnsafeCell<T>;
    drop(Box::from_raw(ptr));
}

impl Drop for HeapEntry {
    fn drop(&mut self) {
        debug_assert!(self.header.strong_count == 0);
        unsafe { self.drop_data_unchecked() }
    }
}

pub struct Heap {
    pub entries: Vec<HeapEntry>,
    pub first_vacant: Option<usize>,
    pub first_taken: Option<usize>,
    pub vacant_entries: usize,
    pub taken_entries: usize,
}

const MEMORY_LIMIT: usize = 2050;

fn get_initialized_heap_storage(initial_size: usize) -> Vec<HeapEntry> {
    let mut heap = Vec::with_capacity(initial_size);
    let header = Header::new();
    heap.push(HeapEntry { data: None, header });
    while heap.len() < heap.capacity() {
        let prev = heap.len() - 1;
        let header = Header::with_next(Some(prev));
        heap.push(HeapEntry { data: None, header });
    }
    heap
}

// This one is safe as it's the heap that owns all of the
// allocated values so it makes sense to bind reference
// lifetimes and mutability with their owner.
unsafe impl ScopeGuard for Heap {}

impl Heap {
    // Creates new heap with capacity one.
    //
    // Note that creating empty heap doesn't really make sense.
    // Capacity1 ensures that we can grow heap by doubling its size
    // and don't need to check for 0 which saves sme jumps.
    pub fn new() -> Self {
        Self::with_capacity(1)
    }

    pub fn with_capacity(capacity: usize) -> Self {
        if capacity == 0 {
            panic!("heaps capacity should be more than 0");
        }
        Self {
            entries: get_initialized_heap_storage(capacity),
            first_taken: None,
            first_vacant: Some(capacity - 1),
            taken_entries: 0,
            vacant_entries: capacity,
        }
    }

    pub fn allocate<T: Allocable>(&mut self, val: T) -> Root<T> {
        let data = Self::heap_allocate(val);
        let header = Header {
            marked: false,
            size: std::mem::size_of::<T>(),
            tag: T::tag(),
            next_node: self.first_taken,
            strong_count: 1,
            weak_count: 0,
            dropped: false,
        };
        let ptr = unsafe { ptr::NonNull::new_unchecked(data as *mut UnsafeCell<()>) };
        let entry = HeapEntry {
            data: Some(ptr.clone()),
            header,
        };
        let entry_index = self.insert_entry(entry);
        Root {
            data: unsafe { ptr::NonNull::new_unchecked(data) },
            entry_index,
            _phantom: PhantomData,
        }
    }

    fn insert_entry(&mut self, entry: HeapEntry) -> usize {
        match self.first_vacant {
            None => {
                self.grow();
                self.insert_entry(entry)
            }
            Some(index) => {
                self.first_vacant = self.entries[index].header.next_node;
                self.entries[index] = entry;
                if let Some(_) = self.first_taken {
                    self.entries[index].header.next_node = self.first_taken;
                }
                self.first_taken = Some(index);
                self.taken_entries += 1;
                self.vacant_entries -= 1;
                index
            }
        }
    }

    fn grow(&mut self) {
        // todo: confirm if this check is reasonable
        assert!(
            self.entries.len() * 2 < MEMORY_LIMIT,
            "Went past set memory limit on heap, probably we have a leak"
        );
        self.entries.reserve(self.entries.len());
        let header = Header::new();
        self.entries.push(HeapEntry { data: None, header });
        while self.entries.len() < self.entries.capacity() {
            let prev = self.entries.len() - 1;
            let header = Header::with_next(Some(prev));
            self.entries.push(HeapEntry { data: None, header });
        }
        self.first_vacant = Some(self.entries.len() - 1);
        self.vacant_entries = self.entries.len() - self.taken_entries;
    }

    pub fn mutate<T, R, Func, Ptr>(&mut self, ptr: &mut Ptr, func: Func) -> R
    where
        Func: FnOnce(&mut T) -> R,
        Ptr: ScopedRef<T> + HeapMarked,
    {
        let mut reference = self.deref_ptr_mut(ptr);
        func(&mut reference)
    }

    #[cfg(debug)]
    pub fn deref_ptr<'a, T, Ptr>(&'a self, ptr: &'a Ptr) -> ScopedPtr<T>
    where
        Ptr: ScopedRef<T> + HeapMarked,
    {
        check_ptr!(self, ptr);
        ScopedPtr {
            value: ptr.scoped_ref(self),
        }
    }

    #[cfg(not(debug))]
    pub fn deref_ptr<'a, T>(&'a self, ptr: &'a impl ScopedRef<T>) -> ScopedPtr<T> {
        ScopedPtr {
            value: ptr.scoped_ref(self),
        }
    }

    #[cfg(debug)]
    pub fn deref_ptr_mut<'a, T, Ptr>(&'a mut self, ptr: &'a mut Ptr) -> ScopedMutPtr<T>
    where
        Ptr: ScopedRef<T> + HeapMarked,
    {
        check_ptr!(self, ptr);
        ScopedMutPtr {
            value: ptr.scoped_ref_mut(self),
        }
    }

    #[cfg(not(debug))]
    pub fn deref_ptr_mut<'a, T>(&'a mut self, ptr: &'a mut impl ScopedRef<T>) -> ScopedMutPtr<T> {
        ScopedMutPtr {
            value: ptr.scoped_ref_mut(self),
        }
    }

    fn heap_allocate<T>(val: T) -> *mut UnsafeCell<T> {
        let ptr = Box::new(UnsafeCell::new(val));
        Box::into_raw(ptr)
    }

    pub fn clone_root<T>(&mut self, ptr: &Root<T>) -> Root<T> {
        self.increment_strong_count(ptr.entry_index);
        Root {
            data: ptr.data.clone(),
            entry_index: ptr.entry_index,
            _phantom: PhantomData,
        }
    }

    pub fn clone_weak<T>(&mut self, ptr: &Weak<T>) -> Weak<T> {
        ptr.clone()
    }

    pub fn downgrade<T>(&mut self, ptr: Root<T>) -> Weak<T> {
        let weak = Weak {
            data: ptr.data.clone(),
            entry_index: ptr.entry_index,
        };
        self.drop_root(ptr);
        weak
    }

    pub fn upgrade<T>(&mut self, ptr: Weak<T>) -> Root<T> {
        self.increment_strong_count(ptr.entry_index);
        Root {
            data: ptr.data,
            entry_index: ptr.entry_index,
            _phantom: PhantomData,
        }
    }

    pub fn drop_root<T>(&mut self, ptr: Root<T>) {
        self.decrement_strong_count(ptr.entry_index);
        std::mem::forget(ptr);
    }

    pub fn free_entry(&mut self, entry_index: usize) {
        // todo: test
        let entry = &mut self.entries[entry_index];
        debug_assert_eq!(entry.header.strong_count, 0, "freeing rooted value");
        debug_assert!(!entry.header.dropped, "double free");

        entry.drop_data();
        entry.header.dropped = true;

        self.vacant_entries += 1;
        self.taken_entries -= 1;

        let next_taken_entry = entry.header.next_node;

        entry.header.next_node = self.first_vacant;
        self.first_vacant = Some(entry_index);

        let (mut curr, mut last_taken) = match self.first_taken {
            None => unreachable!("trying to free on an empty heap will panic earlier on data drop"),
            Some(next_index) if next_index == entry_index => {
                self.first_taken = next_taken_entry;
                return;
            }
            Some(next_index) => (Some(next_index), next_index),
        };
        if let None = next_taken_entry {
            // Return early as there is no point in heap traversal to
            // add an empty tail
            return;
        }
        while let Some(index) = curr {
            last_taken = index;
            curr = self.entries[index].header.next_node;
        }
        self.entries[last_taken].header.next_node = next_taken_entry;
    }

    fn increment_strong_count(&mut self, entry_index: usize) {
        self.with_header(entry_index, |header| header.strong_count += 1);
    }

    fn decrement_strong_count(&mut self, entry_index: usize) {
        self.with_header(entry_index, |header| header.strong_count -= 1);
    }

    fn with_header<F: Fn(&mut Header) -> ()>(&mut self, entry_index: usize, func: F) {
        let header = &mut self.entries[entry_index].header;
        func(header);
    }
}

pub struct MarkSweep;

impl MarkSweep {
    pub fn new() -> Self {
        Self
    }

    pub fn step(
        &mut self,
        heap: &mut Heap,
        call_stack: &Vec<FuncFrame>,
        modules: &HashMap<String, ModuleState>,
    ) {
        self.mark(heap, call_stack, modules);
        self.sweep(heap);
        let mut curr = heap.first_taken;
        while let Some(entry_index) = curr {
            heap.entries[entry_index].header.marked = false;
            curr = heap.entries[entry_index].header.next_node;
        }
    }

    fn mark(
        &mut self,
        heap: &mut Heap,
        call_stack: &Vec<FuncFrame>,
        modules: &HashMap<String, ModuleState>,
    ) {
        if heap.taken_entries == 0 {
            return;
        }
        // mark roots
        let mut marked = self.traverse_and_mark(heap);
        // call stack
        self.visit_call_stack(&mut marked, heap, call_stack);
        // modules
        for (_, module_state) in modules.iter() {
            if let ModuleState::Evaluated(module_env) = module_state {
                self.visit_env(&mut marked, heap, module_env.clone());
            }
        }
        heap.taken_entries = marked.len();
        heap.vacant_entries = heap.entries.len() - marked.len();
    }

    fn visit_call_stack(
        &self,
        marked: &mut Set<usize>,
        heap: &mut Heap,
        call_stack: &Vec<FuncFrame>,
    ) {
        for frame in call_stack {
            self.visit_env(marked, heap, frame.globals.clone());
            if let Some(locals) = frame.locals.clone() {
                self.visit_env(marked, heap, locals);
            }
        }
    }

    fn traverse_and_mark(&self, heap: &mut Heap) -> Set<usize> {
        let mut marked = Set::new();
        let mut curr = heap.first_taken;

        const N: usize = 5;
        let mut last_n = Vec::new();
        while let Some(entry_index) = curr {
            // println!("Current index {}", entry_index);
            if last_n.len() < N {
                last_n.push(entry_index);
            } else {
                if last_n.iter().all(|x| *x == last_n[0]) {
                    panic!("Last n visits are the same {}", last_n[0]);
                } else {
                    last_n.push(entry_index);
                    last_n.remove(0);
                }
            }
            if heap.entries[entry_index].header.strong_count > 0 {
                // println!("String count is higher than 0 we enter");
                self.visit_entry(&mut marked, heap, entry_index);
            }
            curr = heap.entries[entry_index].header.next_node;
        }
        marked
    }

    fn visit_entry(&self, marked: &mut Set<usize>, heap: &mut Heap, entry_index: usize) {
        if marked.contains(&entry_index) {
            return;
        }
        marked.insert(entry_index);
        let header = &mut heap.entries[entry_index].header;
        header.marked = true;
        let walk_function = match header.tag {
            TypeTag::List => Self::visit_list,
            TypeTag::Lambda => Self::visit_lambda,
            TypeTag::RuntimeFunc => Self::visit_func,
            // We don't use catch all here so if we add any other type this
            // will stop compiling and its a good thing
            TypeTag::None | TypeTag::String => {
                return;
            }
        };
        walk_function(self, marked, heap, entry_index);
    }

    fn visit_func(&self, marked: &mut Set<usize>, heap: &mut Heap, entry_index: usize) {
        // todo: remove unsafe usage here
        let func_ref = heap.entries[entry_index].data.unwrap().as_ptr() as *const RuntimeFunc;
        let func_ref = unsafe { func_ref.as_ref().unwrap() };
        self.visit_weak_val(marked, heap, &func_ref.body);
    }

    fn visit_list(&self, marked: &mut Set<usize>, heap: &mut Heap, entry_index: usize) {
        // todo: remove unsafe usage here
        let list_ref = heap.entries[entry_index].data.unwrap().as_ptr() as *const Vec<WeakVal>;
        let list_ref = unsafe { list_ref.as_ref().unwrap() };
        list_ref
            .iter()
            .for_each(|val| self.visit_weak_val(marked, heap, val));
    }

    fn visit_lambda(&self, marked: &mut Set<usize>, heap: &mut Heap, entry_index: usize) {
        // todo: remove unsafe usage here
        let lambda_ref = heap.entries[entry_index].data.unwrap().as_ptr() as *const Lambda;
        let lambda_ref = unsafe { lambda_ref.as_ref().unwrap() };
        self.visit_env(marked, heap, lambda_ref.env.clone());
        self.visit_weak_val(marked, heap, &lambda_ref.body);
    }

    fn visit_env(&self, marked: &mut Set<usize>, heap: &mut Heap, env: Environment) {
        let parent;
        {
            let inner = env.borrow();
            for val in inner.values.values() {
                self.visit_weak_val(marked, heap, val);
            }
            parent = inner.parent.clone();
        }
        if let Some(parent_inner) = parent {
            self.visit_env(marked, heap, parent_inner)
        }
    }

    fn visit_weak_val(&self, marked: &mut Set<usize>, heap: &mut Heap, val: &WeakVal) {
        use WeakVal::*;
        match val {
            Func(ptr) => self.visit_entry(marked, heap, ptr.entry_index),
            Lambda(ptr) => self.visit_entry(marked, heap, ptr.entry_index),
            List(ptr) => self.visit_entry(marked, heap, ptr.entry_index),
            StringVal(ptr) => self.visit_entry(marked, heap, ptr.entry_index),
            // We don't use catch all here so if we add any other type this
            // will stop compiling and its a good thing
            NumberVal(_) | Symbol(_) | NativeFunc(_) => (),
        }
    }

    fn sweep(&mut self, heap: &mut Heap) {
        let first_marked = self.free_from_head(heap);
        heap.first_taken = first_marked;
        let curr = match first_marked {
            None => return,
            Some(index) => index,
        };
        let mut last_marked = curr;
        let mut curr = heap.entries[curr].header.next_node;
        while let Some(entry_index) = curr {
            let marked = heap.entries[entry_index].header.marked;
            if marked {
                let entry = &mut heap.entries[entry_index];
                last_marked = entry_index;
                curr = entry.header.next_node;
                continue;
            }
            print_debug_value(heap, entry_index);
            let entry = &mut heap.entries[entry_index];
            curr = entry.header.next_node;
            entry.drop_data();
            entry.header.dropped = true;
            entry.header.next_node = heap.first_vacant;
            heap.first_vacant = Some(entry_index);

            heap.entries[last_marked].header.next_node = curr;
        }
    }

    fn free_from_head(&mut self, heap: &mut Heap) -> Option<usize> {
        let mut curr = heap.first_taken?;
        while !heap.entries[curr].header.marked {
            print_debug_value(heap, curr);
            let entry = &mut heap.entries[curr];
            entry.drop_data();
            let next_node = entry.header.next_node;
            entry.header.next_node = heap.first_vacant;
            heap.first_vacant = Some(curr);
            // we removed all unmarked marked entries
            // we can break early
            curr = next_node?;
        }
        // Current entry is not in marked
        // we need to start removing from the middle
        Some(curr)
    }
}

fn print_debug_value(heap: &mut Heap, entry_index: usize) {
    let entry = &heap.entries[entry_index];
    let _to_print = match entry.header.tag {
        TypeTag::Lambda => "[GC] Lambda is dropped".to_owned(),
        TypeTag::None => "[GC] No type tag for the value".to_owned(),
        TypeTag::RuntimeFunc => unsafe {
            let ptr = entry.data.unwrap().as_ptr() as *const runtime::RuntimeFunc;
            let as_ref = ptr.as_ref().unwrap();
            format!("[GC] Dropping function with name {}", as_ref.name)
        },
        TypeTag::String => unsafe {
            let ptr = entry.data.unwrap().as_ptr() as *const String;
            let copy = ptr.as_ref().unwrap().clone();
            format!("[GC] Dropping string {}", &copy)
        },
        TypeTag::List => unsafe {
            let ptr = entry.data.unwrap().as_ptr() as *const Vec<WeakVal>;
            let msg: Vec<String> = ptr
                .as_ref()
                .unwrap()
                .iter()
                .map(|x| x.simple_repr(heap))
                .collect();
            let msg = msg.concat();
            format!("[GC] Dropping list: {}", &msg)
        },
    };
    // println!("{}", to_print);
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::runtime::RootedVal;

    #[test]
    fn empty_heap_has_nonzero_capacity() {
        let heap = Heap::new();
        assert_ne!(heap.entries.len(), 0);
    }

    #[test]
    fn new_heap_has_correct_capacity() {
        let cap = 10;
        let heap = Heap::with_capacity(cap);
        assert_eq!(heap.entries.len(), cap);
    }

    #[test]
    #[should_panic]
    fn heap_with_capacity_zero_panics() {
        Heap::with_capacity(0);
    }

    #[test]
    fn new_heap_has_vacant_entry_set() {
        let heap = Heap::with_capacity(10);
        assert!(heap.first_vacant.is_some());
    }

    #[test]
    fn new_heap_has_not_set_taken_entry() {
        let heap = Heap::with_capacity(10);
        assert!(heap.first_taken.is_none());
    }

    #[test]
    fn new_heap_has_no_taken_entries() {
        let heap = Heap::with_capacity(10);
        assert!(heap.entries.iter().all(|e| e.data.is_none()))
    }

    #[test]
    fn new_heap_vacant_entries_are_linked() {
        let cap = 10;
        let heap = Heap::with_capacity(cap);
        let mut curr = heap.first_vacant;
        let mut vacant_entries = Vec::new();
        while let Some(i) = curr {
            vacant_entries.push(i);
            curr = heap.entries[i].header.next_node;
        }
        vacant_entries.dedup();
        assert_eq!(vacant_entries.len(), cap);
    }

    #[test]
    fn allocating_entry_sets_first_taken_to_first_vacant() {
        let mut heap = Heap::new();
        let first_vacant = heap.first_vacant.clone();
        let res = heap.allocate(String::new());
        assert_eq!(first_vacant, heap.first_taken);
        heap.drop_root(res);
    }

    #[test]
    fn allocating_last_vacant_entry_in_heap_sets_vacant_entry_to_none() {
        let mut heap = Heap::new();
        let res = heap.allocate(String::new());
        assert_eq!(heap.first_vacant, None);
        heap.drop_root(res);
    }

    #[test]
    fn allocating_with_multiple_vacant_entries_moves_vacant_entry_to_the_next() {
        let mut heap = Heap::with_capacity(10);
        let next_vacant = heap.entries[heap.first_vacant.unwrap()].header.next_node;
        let ptr = heap.allocate(String::new());
        assert_eq!(heap.first_vacant, next_vacant);
        heap.drop_root(ptr);
    }

    #[test]
    fn first_allocated_heap_entry_has_next_node_as_none() {
        let mut heap = Heap::with_capacity(10);
        let ptr = heap.allocate(String::new());
        assert_eq!(heap.entries[ptr.entry_index].header.next_node, None);
        heap.drop_root(ptr);
    }

    #[test]
    fn second_allocated_entry_points_to_the_first_entry() {
        let mut heap = Heap::with_capacity(10);
        let ptr1 = heap.allocate(String::new());
        let ptr2 = heap.allocate(String::new());
        assert_eq!(
            heap.entries[ptr2.entry_index].header.next_node.unwrap(),
            ptr1.entry_index
        );
        heap.drop_root(ptr1);
        heap.drop_root(ptr2);
    }

    #[test]
    fn changing_data_through_gc_ptr_changes_data_in_heap_entry() {
        let mut heap = Heap::with_capacity(10);
        let mut ptr1 = heap.allocate(Vec::new());
        unsafe { ptr1.data.as_mut().get_mut().push(WeakVal::NumberVal(2.0)) }
        let data_ref = *(&heap.entries[ptr1.entry_index]
            .data
            .as_ref()
            .unwrap()
            .as_ptr());
        let data_ref = data_ref as *const Vec<RootedVal>;
        let data_ref = unsafe { data_ref.as_ref().unwrap() };
        let heap_entry_val = match data_ref[0] {
            RootedVal::NumberVal(x) => x,
            _ => panic!("that should not happen"),
        };
        assert_eq!(data_ref.len(), 1);
        assert_eq!(heap_entry_val, 2.0);
        heap.drop_root(ptr1);
    }

    #[test]
    fn cloned_gc_pointers_point_to_the_same_data() {
        let mut heap = Heap::with_capacity(10);
        let ptr1 = heap.allocate(String::new());
        let ptr2 = heap.clone_root(&ptr1);
        assert_eq!(ptr1.entry_index, ptr2.entry_index);
        assert_eq!(ptr1.data.as_ptr(), ptr2.data.as_ptr());
        heap.drop_root(ptr1);
        heap.drop_root(ptr2);
    }

    #[test]
    fn heap_size_at_lest_doubles_after_allocating_on_full_heap() {
        const INITIAL_HEAP_CAPACITY: usize = 10;
        let mut heap = Heap::with_capacity(INITIAL_HEAP_CAPACITY);
        let mut allocs = Vec::new();
        for _ in 0..=INITIAL_HEAP_CAPACITY {
            allocs.push(heap.allocate(String::new()));
        }
        assert!(heap.entries.len() >= INITIAL_HEAP_CAPACITY * 2);
        allocs.into_iter().for_each(|x| heap.drop_root(x));
    }

    #[test]
    fn full_heap_after_growing_has_vacant_entries() {
        // we need to have at least capacity 2 for this test to work
        // as capacity 1 heap could frow to 2 in allocation and immediately
        // take the only vacant entry, or it could grow more as `reserve` on `Vec`
        // can give us more if it wants to.
        let mut heap = Heap::with_capacity(2);
        let ptr1 = heap.allocate(String::new());
        let ptr2 = heap.allocate(String::new());
        let ptr3 = heap.allocate(String::new());
        assert_eq!(heap.first_vacant.unwrap(), heap.entries.len() - 2);
        heap.drop_root(ptr1);
        heap.drop_root(ptr2);
        heap.drop_root(ptr3);
    }

    #[test]
    fn full_heap_after_growing_does_not_invalidate_pointers() {
        let mut heap = Heap::with_capacity(1);
        let mut ptr = heap.allocate(Vec::new());
        let ptr2 = heap.allocate(String::new());
        unsafe { ptr.data.as_mut().get_mut().push(WeakVal::NumberVal(10.0)) };
        let data_ref = *(&heap.entries[ptr.entry_index]
            .data
            .as_ref()
            .unwrap()
            .as_ptr());
        let data_ref = data_ref as *const Vec<RootedVal>;
        let data_ref = unsafe { data_ref.as_ref().unwrap() };
        let heap_entry_val = match data_ref[0] {
            RootedVal::NumberVal(x) => x,
            _ => panic!("that should not happen"),
        };
        assert_eq!(data_ref.len(), 1);
        assert_eq!(heap_entry_val, 10.0);
        heap.drop_root(ptr);
        heap.drop_root(ptr2);
    }

    #[test]
    fn heap_allocation_preserves_data() {
        let mut heap = Heap::new();
        let ptr = heap.allocate("Hello".to_string());
        assert_eq!(unsafe { (&*ptr.data.as_ref().get()).clone() }, "Hello");
        heap.drop_root(ptr);
    }
}
