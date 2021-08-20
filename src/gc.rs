use std::{
    cell::{Cell, UnsafeCell},
    marker::PhantomData,
    ops::{Deref, DerefMut},
    ptr,
};

#[cfg(feature = "hashbrown")]
use hashbrown::HashMap;
#[cfg(not(feature = "hashbrown"))]
use std::collections::HashMap;

use crate::{
    env::Environment,
    eval::{FuncFrame, ModuleState},
    native::{NativeStruct, VirtualTable},
    runtime::{self, Lambda, WeakVal},
};

#[cfg(all(feature = "hash_set", feature = "hashbrown"))]
pub type Set<T> = hashbrown::HashSet<T>;

#[cfg(all(feature = "hash_set", not(feature = "hashbrown")))]
pub type Set<T> = std::collections::HashSet<T>;

#[cfg(not(feature = "hash_set"))]
pub type Set<T> = std::collections::BTreeSet<T>;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum TypeTag {
    Lambda,
    List,
    String,
    Boxed,
    UserType,
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

pub struct ScopedPtr<'guard, T: ?Sized> {
    pub value: &'guard T,
}

impl<T: ?Sized> Deref for ScopedPtr<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.value
    }
}

pub struct ScopedMutPtr<'guard, T: ?Sized> {
    pub value: &'guard mut T,
}

impl<T: ?Sized> Deref for ScopedMutPtr<'_, T> {
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

pub trait ScopedRef<T: ?Sized> {
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

/// Represents allocated value alongside header information.
///
/// repr(C) is required here because we always want to have header information
/// layed out the same way starting from the beginning of the struct
/// regardless of the contained type. This way the alignment of the struct
/// might change, but the offsets from the start to all of the fields can't
/// So we can safely access header information on erased types.
#[repr(C)]
pub struct RawPtrBox<T: ?Sized> {
    pub entry_index: usize,
    pub strong_count: Cell<usize>,
    pub value: UnsafeCell<T>,
}

impl<T: ?Sized> RawPtrBox<T> {
    pub fn update_strong_count<F: FnMut(usize) -> usize>(&self, mut func: F) {
        let old_val = self.strong_count.get();
        let new_val = func(old_val);
        self.strong_count.set(new_val);
    }
}

#[repr(transparent)]
pub struct Root<T: ?Sized> {
    ptr: Cell<ptr::NonNull<RawPtrBox<T>>>,
    _phantom: PhantomData<T>,
}

impl<T> PartialEq for Root<T> {
    fn eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr
    }
}

impl<T> Root<T> {
    pub unsafe fn inner_cell(&self) -> *const UnsafeCell<T> {
        &self.ptr.get().as_ref().value as *const UnsafeCell<T>
    }

    /// Casts `Root` between types.
    ///
    /// User needs to make sure that the pointer data after
    /// type cast is properly aligned.
    /// If that is not uphold then dereferencing the pointer is UB.
    pub unsafe fn cast<U>(self) -> Root<U> {
        let new_ptr = Root {
            ptr: Cell::new(self.ptr.get().cast::<RawPtrBox<U>>()),
            _phantom: PhantomData,
        };
        std::mem::forget(self);
        new_ptr
    }

    pub fn downgrade(self) -> Weak<T> {
        unsafe { self.ptr.get().as_ref().update_strong_count(|x| x - 1) };
        let res = Weak {
            ptr: Cell::new(self.ptr.get()),
        };
        std::mem::forget(self);
        res
    }

    /// Swaps pointers inside two Roots of the same type.
    pub fn swap(&self, other: &Root<T>) {
        let tmp = self.ptr.get();
        self.ptr.set(other.ptr.get());
        other.ptr.set(tmp);
    }
}

impl<T> Eq for Root<T> {}

impl<T> Clone for Root<T> {
    fn clone(&self) -> Self {
        unsafe {
            self.ptr.get().as_ref().update_strong_count(|x| x + 1);
        }
        Root {
            ptr: self.ptr.clone(),
            _phantom: PhantomData,
        }
    }
}

impl<T: ?Sized> ScopedRef<T> for Root<T> {
    fn scoped_ref<'a>(&'a self, _guard: &'a dyn ScopeGuard) -> &'a T {
        // This is safe because the only way to call this method is to pass
        // a guard that has the same borrow as this ref.
        // So we cannot borrow mutably if we borrow guard mutably at the same time.
        // The ScopeGuard trait is an unsafe trait only implemented for
        // a Heap struct in this module which cannot be cloned during
        // mutator's execution.
        // If the user implements ScopeGuard themselves it's their responsibility
        // to ensure that the rust invariants will be upheld.
        unsafe { &*self.ptr.get().as_ref().value.get() }
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
        unsafe { &mut *self.ptr.get().as_ref().value.get() }
    }
}

impl<T: ?Sized> HeapMarked for Root<T> {
    fn entry_index(&self) -> usize {
        unsafe { self.ptr.get().as_ref().entry_index }
    }
}

impl<T: ?Sized> Drop for Root<T> {
    fn drop(&mut self) {
        unsafe {
            assert!(
                self.ptr.get().as_ref().strong_count.get() > 0,
                "Dropping root ptr with 0 strong count"
            );
            self.ptr.get().as_ref().update_strong_count(|x| x - 1)
        }
    }
}

#[repr(transparent)]
pub struct Weak<T: ?Sized> {
    ptr: Cell<ptr::NonNull<RawPtrBox<T>>>,
}

impl<T: ?Sized> Weak<T> {
    pub fn upgrade(self) -> Root<T> {
        unsafe {
            self.ptr.get().as_ref().update_strong_count(|x| x + 1);
            Root {
                ptr: self.ptr,
                _phantom: PhantomData,
            }
        }
    }

    // Swaps pointers between two Weak of the same type.
    pub fn swap(&self, other: &Weak<T>) {
        let tmp = self.ptr.get();
        self.ptr.set(other.ptr.get());
        other.ptr.set(tmp);
    }
}

impl<T> PartialEq for Weak<T> {
    fn eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr
    }
}

impl<T> Eq for Weak<T> {}

impl<T: ?Sized> ScopedRef<T> for Weak<T> {
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
        unsafe { &*self.ptr.get().as_ref().value.get() }
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
        unsafe { &mut *self.ptr.get().as_ref().value.get() }
    }
}

impl<T: ?Sized> HeapMarked for Weak<T> {
    fn entry_index(&self) -> usize {
        unsafe { self.ptr.get().as_ref().entry_index }
    }
}

impl<T: ?Sized> Clone for Weak<T> {
    fn clone(&self) -> Self {
        Self {
            ptr: self.ptr.clone(),
        }
    }
}

pub trait Allocable {
    fn tag() -> TypeTag;
}

pub struct Header {
    pub size: usize,
    pub tag: TypeTag,
    pub marked: Cell<bool>,
    pub next_node: Option<usize>,
    pub dropped: bool,
    pub vptr: Option<&'static VirtualTable>,
}

impl Header {
    pub fn new() -> Self {
        Self {
            size: 0,
            tag: TypeTag::None,
            marked: Cell::new(false),
            next_node: None,
            dropped: false,
            vptr: None,
        }
    }
    pub fn with_next(next: Option<usize>) -> Self {
        Self {
            size: 0,
            tag: TypeTag::None,
            marked: Cell::new(false),
            next_node: next,
            dropped: false,
            vptr: None,
        }
    }
}

pub struct HeapEntry {
    pub data: Option<ptr::NonNull<RawPtrBox<()>>>,
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
                    safe_drop_and_free::<runtime::Lambda>(ptr.as_ptr());
                }
                TypeTag::List => {
                    safe_drop_and_free::<Vec<WeakVal>>(ptr.as_ptr());
                }
                TypeTag::String => {
                    safe_drop_and_free::<String>(ptr.as_ptr());
                }
                TypeTag::Boxed => {
                    safe_drop_and_free::<runtime::WeakVal>(ptr.as_ptr());
                }
                TypeTag::UserType => match self.header.vptr {
                    None => unreachable!("User type should always have drop function implemented"),
                    Some(vptr) => (vptr.drop)(ptr.as_ptr()),
                },
                TypeTag::None => (),
            },
        }
    }

    pub fn drop_data(&mut self) {
        if let None = self.data {
            panic!("Free of an empty heap entry");
        }
        match self.data {
            None => panic!("Free of an empty heap entry"),
            Some(data_ptr) => unsafe {
                debug_assert!(data_ptr.as_ref().strong_count.get() == 0);
                self.drop_data_unchecked();
                self.data = None;
            },
        }
    }

    pub unsafe fn data_ref<T>(&self) -> &T {
        debug_assert!(
            !self.header.dropped,
            "Cannot take reference to dropped data"
        );
        let ptr = &self.data.expect("Dereferencing free entry");
        let ptr = ptr.as_ptr() as *const RawPtrBox<T>;
        &*(*ptr).value.get()
    }

    pub fn strong_count(&self) -> usize {
        match self.data {
            None => panic!("Checking strong count of empty entry"),
            Some(data_ptr) => unsafe { data_ptr.as_ref().strong_count.get() },
        }
    }

    pub fn modify_strong_count<F: FnMut(usize) -> usize>(&self, func: F) {
        match self.data {
            None => panic!("Modifying strong count on empty entry is an error"),
            Some(data_ptr) => unsafe { data_ptr.as_ref().update_strong_count(func) },
        }
    }
}

unsafe fn safe_drop_and_free<T>(ptr: *mut RawPtrBox<()>) {
    let ptr = ptr as *mut RawPtrBox<T>;
    drop(Box::from_raw(ptr));
}

impl Drop for HeapEntry {
    fn drop(&mut self) {
        match self.data {
            None => (),
            Some(data_ptr) => unsafe {
                debug_assert!(data_ptr.as_ref().strong_count.get() == 0);
                self.drop_data_unchecked()
            },
        }
    }
}

pub struct Heap {
    pub entries: Vec<HeapEntry>,
    pub first_vacant: Option<usize>,
    pub first_taken: Option<usize>,
    pub vacant_entries: Cell<usize>,
    pub taken_entries: Cell<usize>,
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
            taken_entries: 0.into(),
            vacant_entries: capacity.into(),
        }
    }
    pub fn allocate_user_type<T: Allocable + NativeStruct>(&mut self, val: T) -> Root<T> {
        self._allocate(val, Some(T::vptr()))
    }

    pub fn allocate<T: Allocable>(&mut self, val: T) -> Root<T> {
        if let TypeTag::UserType = T::tag() {
            panic!("Use allocate_user_type for user defined types");
        }
        self._allocate(val, None)
    }

    fn _allocate<T: Allocable>(&mut self, val: T, vptr: Option<&'static VirtualTable>) -> Root<T> {
        let header = Header {
            marked: Cell::new(false),
            size: std::mem::size_of::<T>(),
            tag: T::tag(),
            next_node: self.first_taken,
            dropped: false,
            vptr,
        };
        let entry = HeapEntry { data: None, header };
        let entry_index = self.insert_entry(entry);
        let data = Self::heap_allocate(val, entry_index);

        let ptr = unsafe { ptr::NonNull::new_unchecked(data as *mut RawPtrBox<()>) };
        self.entries[entry_index].data = Some(ptr.clone());
        unsafe { (*data).strong_count.set(1) };
        Root {
            ptr: Cell::new(unsafe { ptr::NonNull::new_unchecked(data) }),
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
                self.taken_entries.set(self.taken_entries.get() + 1);
                self.vacant_entries.set(self.vacant_entries.get() - 1);
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
        self.vacant_entries
            .set(self.entries.len() - self.taken_entries.get());
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
        T: ?Sized,
        Ptr: ScopedRef<T> + HeapMarked,
    {
        check_ptr!(self, ptr);
        ScopedPtr {
            value: ptr.scoped_ref(self),
        }
    }

    #[cfg(not(debug))]
    pub fn deref_ptr<'a, T: ?Sized>(&'a self, ptr: &'a impl ScopedRef<T>) -> ScopedPtr<T> {
        ScopedPtr {
            value: ptr.scoped_ref(self),
        }
    }

    #[cfg(debug)]
    pub fn deref_ptr_mut<'a, T, Ptr>(&'a mut self, ptr: &'a mut Ptr) -> ScopedMutPtr<T>
    where
        T: ?Sized,
        Ptr: ScopedRef<T> + HeapMarked,
    {
        check_ptr!(self, ptr);
        ScopedMutPtr {
            value: ptr.scoped_ref_mut(self),
        }
    }

    #[cfg(not(debug))]
    pub fn deref_ptr_mut<'a, T: ?Sized>(
        &'a mut self,
        ptr: &'a mut impl ScopedRef<T>,
    ) -> ScopedMutPtr<T> {
        ScopedMutPtr {
            value: ptr.scoped_ref_mut(self),
        }
    }

    fn heap_allocate<T>(val: T, entry_index: usize) -> *mut RawPtrBox<T> {
        let ptr = Box::new(RawPtrBox {
            entry_index,
            strong_count: Cell::new(0),
            value: UnsafeCell::new(val),
        });
        Box::into_raw(ptr)
    }

    pub fn free_entry(&mut self, entry_index: usize) {
        // todo: test
        let entry = &mut self.entries[entry_index];
        debug_assert_eq!(entry.strong_count(), 0, "freeing rooted value");
        debug_assert!(!entry.header.dropped, "double free");

        entry.drop_data();
        entry.header.dropped = true;

        self.taken_entries.set(self.taken_entries.get() - 1);
        self.vacant_entries.set(self.vacant_entries.get() + 1);

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
            heap.entries[entry_index].header.marked.set(false);
            curr = heap.entries[entry_index].header.next_node;
        }
    }

    fn mark(
        &mut self,
        heap: &Heap,
        call_stack: &Vec<FuncFrame>,
        modules: &HashMap<String, ModuleState>,
    ) {
        if heap.taken_entries.get() == 0 {
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
        heap.taken_entries.set(marked.len());
        heap.vacant_entries.set(heap.entries.len() - marked.len());
    }

    fn visit_call_stack(&self, marked: &mut Set<usize>, heap: &Heap, call_stack: &Vec<FuncFrame>) {
        for frame in call_stack {
            self.visit_env(marked, heap, frame.globals.clone());
            if let Some(locals) = frame.locals.clone() {
                self.visit_env(marked, heap, locals);
            }
        }
    }

    fn traverse_and_mark(&self, heap: &Heap) -> Set<usize> {
        let mut marked = Set::new();
        let mut curr = heap.first_taken;

        const N: usize = 5;
        let mut last_n = Vec::new();
        while let Some(entry_index) = curr {
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
            if heap.entries[entry_index].strong_count() > 0 {
                self.visit_entry(&mut marked, heap, entry_index);
            }
            curr = heap.entries[entry_index].header.next_node;
        }
        marked
    }

    fn visit_entry(&self, marked: &mut Set<usize>, heap: &Heap, entry_index: usize) {
        if marked.contains(&entry_index) {
            return;
        }
        marked.insert(entry_index);
        let header = &heap.entries[entry_index].header;
        header.marked.set(true);
        let walk_function = match header.tag {
            TypeTag::List => Self::visit_list,
            TypeTag::Lambda => Self::visit_lambda,
            TypeTag::UserType => Self::visit_user_type,
            TypeTag::Boxed => Self::visit_boxed,
            // We don't use catch all here so if we add any other type this
            // will stop compiling and its a good thing
            TypeTag::None | TypeTag::String => {
                return;
            }
        };
        walk_function(self, marked, heap, entry_index);
    }

    fn visit_boxed(&self, marked: &mut Set<usize>, heap: &Heap, entry_index: usize) {
        let value_ref = Self::entry_data::<runtime::WeakVal>(heap, entry_index);
        self.visit_weak_val(marked, heap, value_ref);
    }

    fn visit_list(&self, marked: &mut Set<usize>, heap: &Heap, entry_index: usize) {
        // todo: remove unsafe usage here
        let list_ref = Self::entry_data::<Vec<WeakVal>>(heap, entry_index);
        list_ref
            .iter()
            .for_each(|val| self.visit_weak_val(marked, heap, val));
    }

    fn visit_lambda(&self, marked: &mut Set<usize>, heap: &Heap, entry_index: usize) {
        // todo: remove unsafe usage here
        let lambda_ref = Self::entry_data::<Lambda>(heap, entry_index);
        self.visit_env(marked, heap, lambda_ref.env.clone());
        self.visit_weak_val(marked, heap, &lambda_ref.body);
    }

    fn visit_user_type(&self, marked: &mut Set<usize>, heap: &Heap, entry_index: usize) {
        let pointer_ref = heap.entries[entry_index].data.unwrap().as_ptr() as *const RawPtrBox<()>;
        let visit_fn = heap.entries[entry_index].header.vptr.unwrap().visit;
        visit_fn(pointer_ref, self, marked, heap);
    }

    fn visit_env(&self, marked: &mut Set<usize>, heap: &Heap, env: Environment) {
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

    fn visit_weak_val(&self, marked: &mut Set<usize>, heap: &Heap, val: &WeakVal) {
        use WeakVal::*;
        match val {
            Macro(ptr) => self.visit_entry(marked, heap, ptr.entry_index()),
            Lambda(ptr) => self.visit_entry(marked, heap, ptr.entry_index()),
            List(ptr) => self.visit_entry(marked, heap, ptr.entry_index()),
            StringVal(ptr) => self.visit_entry(marked, heap, ptr.entry_index()),
            UserType(ptr) => self.visit_entry(marked, heap, ptr.data.entry_index()),
            Boxed { inner, .. } => self.visit_entry(marked, heap, inner.entry_index()),
            // We don't use catch all here so if we add any other type this
            // will stop compiling and its a good thing
            NumberVal(_) | Symbol(_) | NativeFunc(_) => (),
        }
    }

    fn entry_data<'a, T>(heap: &'a Heap, entry_index: usize) -> &'a T {
        unsafe { heap.entries[entry_index].data_ref() }
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
            let marked = heap.entries[entry_index].header.marked.get();
            if marked {
                let entry = &mut heap.entries[entry_index];
                last_marked = entry_index;
                curr = entry.header.next_node;
                continue;
            }
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
        while !heap.entries[curr].header.marked.get() {
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
