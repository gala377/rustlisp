use std::{collections::BTreeSet, ptr};

use crate::{data::Environment, runtime::{self, Lambda, RuntimeVal}};


#[derive(PartialEq, Eq, Clone, Debug)]
pub enum TypeTag {
    Lambda,
    List,
    String,
    RuntimeFunc,
    None,
}

pub struct Gc<T: ?Sized> {
    pub data: ptr::NonNull<T>,
    pub entry_index: usize,
}

impl<T: ?Sized> Clone for Gc<T> {
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
    // todo:
    // we could have those 3 fields as a single usize field where
    // 1st bit is marked flag
    // 2nd bit is rooted flag
    // 3rd bit is does it have a next node
    // rest is the next node index
    pub marked: bool,
    pub rooted: bool,
    pub next_node: Option<usize>,
}

impl Header {
    pub fn new() -> Self {
        Self {
            size: 0,
            tag: TypeTag::None,
            marked: false,
            rooted: false,
            next_node: None,
        }
    }
    pub fn with_next(next: Option<usize>) -> Self {
        Self {
            size: 0,
            tag: TypeTag::None,
            marked: false,
            rooted: false,
            next_node: next,
        }
    }

}

pub struct HeapEntry {
    pub data: Option<ptr::NonNull<()>>,
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
                    println!("Dropping lambda");
                    safe_drop_and_free::<runtime::Lambda>(ptr.as_ptr());
                }
                TypeTag::List => {
                    println!("Dropping list");
                    safe_drop_and_free::<Vec<RuntimeVal>>(ptr.as_ptr());
                }
                TypeTag::RuntimeFunc => {
                    println!("Dropping function");
                    safe_drop_and_free::<runtime::RuntimeFunc>(ptr.as_ptr());
                }
                TypeTag::String => {
                    println!("Dropping string");
                    safe_drop_and_free::<String>(ptr.as_ptr());
                }
                TypeTag::None => (),
            }
        }
    }

    pub fn drop_data(&mut self) {
        if let None = self.data {
            panic!("Free of an empty heap entry");
        }
        unsafe { self.drop_data_unchecked(); }
        self.data = None;
    }
}

unsafe fn safe_drop_and_free<T>(ptr: *mut ()) {
    let ptr = ptr as *mut T;
    std::ptr::drop_in_place(ptr);
    drop(Box::from_raw(ptr));
}

impl Drop for HeapEntry {
    fn drop(&mut self) {
        unsafe { self.drop_data_unchecked() }
    }
}

pub struct Heap {
    pub entries: Vec<HeapEntry>,
    pub first_vacant: Option<usize>,
    pub first_taken: Option<usize>,
    pub vacant_entries: usize,
    pub taken_entries: usize
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

    pub fn allocate<T: Allocable>(&mut self, val: T) -> Gc<T> {
        let data = Self::as_boxed_bytes(val);
        let header = Header {
            marked: false,
            rooted: true,
            size: std::mem::size_of::<T>(),
            tag: T::tag(),
            next_node: self.first_taken,
        };
        let ptr = unsafe { ptr::NonNull::new_unchecked(data as *mut ())};
        let entry = HeapEntry {
            data: Some(ptr.clone()),
            header,
        };
        let entry_index = self.insert_entry(entry);
        let ptr = ptr.as_ptr() as *mut T;
        Gc {
            data: unsafe { ptr::NonNull::new_unchecked(ptr) },
            entry_index,
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

    pub fn as_boxed_bytes<T>(val: T) -> *mut T {
        let ptr = Box::new(val);
        Box::into_raw(ptr)
    }

    pub fn free_entry(&mut self, entry_index: usize) {
        // todo: test
        let entry = &mut self.entries[entry_index];
        entry.drop_data();

        self.vacant_entries += 1;
        self.taken_entries -= 1;

        let entrys_next_taken =  entry.header.next_node;

        entry.header.next_node = self.first_vacant;
        self.first_vacant = Some(entry_index);

        let (mut curr, mut last_taken) = match self.first_taken {
            None => unreachable!("trying to free on an empty heap will panic earlier on data drop"),
            Some(next_index) if next_index == entry_index => {
                self.first_taken = entrys_next_taken;
                return;
            }
            Some(next_index) => (Some(next_index), next_index)
        };
        if let None = entrys_next_taken {
            // Return early as there is no point in heap traversal to
            // add an empty tail
            return;
        }
        while let Some(index) = curr {
            last_taken = index;
            curr = self.entries[index].header.next_node;
        }
        self.entries[last_taken].header.next_node = entrys_next_taken;
    }
}

pub struct MarkSweep;

impl MarkSweep {
    pub fn new() -> Self {
        Self
    }

    pub fn step(&mut self, heap: &mut Heap, _globals: Environment, _locals: Option<Environment>) {
        if heap.taken_entries == 0 {
            return;
        }
        // todo: how do we know something is rooted?
        // what about variables that are on the stack?
        //
        // for example we gc when evaluating
        // (define x ("hello" "world"))
        // ------------------^
        // gc kicks in here
        // so now we have Gc<String> = "hello" on the stack
        // that is neither in the local env, neither in the list
        // (as it does not exist yet).
        // maybe we mark them as young but then what if
        // (define x ("hello"  <long list of values> "world"))
        // GC 1 -------------^
        // GC 2 -------------------------------------------^
        // when gc 1 kicks in "hello" is marked as young so we
        // mark them and move them to the normal entries.
        // But then gc 2 kicks in  and "hello" still did not leave
        // the stack.
        // Maybe we should unroot things and everything
        // is rooted when it starts?
        // Maybe we need explicit stack for temporaries just so
        // we won't deallocate them
        let marked = self.traverse_and_mark(heap);
        heap.taken_entries = marked.len();
        heap.vacant_entries = heap.entries.len() - marked.len();
        self.sweep(heap);
    }

    fn traverse_and_mark(&self, heap: &mut Heap) -> BTreeSet<usize> {
        let mut marked = BTreeSet::new();
        let mut curr = heap.first_taken;
        while let Some(entry_index) = curr {
            if heap.entries[entry_index].header.rooted {
                self.visit_entry(&mut marked, heap, entry_index);
            }
            curr = heap.entries[entry_index].header.next_node;
        }
        marked
    }

    fn visit_entry(&self, marked: &mut BTreeSet<usize>, heap: &mut Heap, entry_index: usize) {
        if marked.contains(&entry_index) {
            return;
        }
        let header = &mut heap.entries[entry_index].header;
        header.marked = true;
        marked.insert(entry_index);
        let walk_function = match header.tag {
            TypeTag::List => Self::visit_list,
            TypeTag::Lambda => Self::visit_lambda,
            // We don't use catch all here so if we add any other type this
            // will stop compiling and its a good thing
            TypeTag::None | TypeTag::RuntimeFunc | TypeTag::String => { return; }
        };
        walk_function(self, marked, heap, entry_index);
    }

    fn visit_list(&self, marked: &mut BTreeSet<usize>, heap: &mut Heap, entry_index: usize) {
        let list_ref = heap.entries[entry_index].data.unwrap().as_ptr() as *const Vec<RuntimeVal>;
        let list_ref = unsafe { list_ref.as_ref().unwrap() };
        list_ref.iter().for_each(|val| self.visit_runtime_val(marked, heap, val));
    }

    fn visit_lambda(&self, marked: &mut BTreeSet<usize>, heap: &mut Heap, entry_index: usize) {
        let lambda_ref = heap.entries[entry_index].data.unwrap().as_ptr() as *const Lambda;
        let lambda_ref = unsafe { lambda_ref.as_ref().unwrap() };
        self.visit_env(marked, heap, lambda_ref.env.clone())
    }

    fn visit_env(&self, marked: &mut BTreeSet<usize>, heap: &mut Heap, env: Environment) {
        #[allow(unused_assignments)]
        let mut parent = None;
        {
            let inner = env.borrow();
            for val in inner.values.values() {
                self.visit_runtime_val(marked, heap, val);
            }
            parent = inner.parent.clone();
        }
        if let Some(parent_inner) = parent {
            self.visit_env(marked, heap, parent_inner)
        }
    }

    fn visit_runtime_val(&self, marked: &mut BTreeSet<usize>, heap: &mut Heap, val: &RuntimeVal) {
        use RuntimeVal::*;
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
        // todo: remove from the head
        let curr = match self.free_from_head(heap) {
            None => return,
            Some(curr) => curr,
        };
        let mut last_marked = curr;
        let mut curr = heap.entries[curr].header.next_node;
        while let Some(entry_index) = curr {
            let entry = &mut heap.entries[entry_index];
            if entry.header.marked {
                last_marked = entry_index;
                curr = entry.header.next_node;
                continue;
            }
            curr = entry.header.next_node;

            entry.drop_data();
            entry.header.next_node = heap.first_vacant;
            heap.first_vacant = Some(entry_index);

            heap.entries[last_marked].header.next_node = curr;
        }
    }

    fn free_from_head(&mut self, heap: &mut Heap) -> Option<usize> {
        let mut curr = heap.first_taken.unwrap();
        while !heap.entries[curr].header.marked {
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

#[cfg(test)]
mod tests {

    use super::*;

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
        heap.allocate(String::new());
        assert_eq!(first_vacant, heap.first_taken);
    }

    #[test]
    fn allocating_last_vacant_entry_in_heap_sets_vacant_entry_to_none() {
        let mut heap = Heap::new();
        heap.allocate(String::new());
        assert_eq!(heap.first_vacant, None);
    }

    #[test]
    fn allocating_with_multiple_vacant_entries_moves_vacant_entry_to_the_next() {
        let mut heap = Heap::with_capacity(10);
        let next_vacant = heap.entries[heap.first_vacant.unwrap()].header.next_node;
        heap.allocate(String::new());
        assert_eq!(heap.first_vacant, next_vacant);
    }

    #[test]
    fn first_allocated_heap_entry_has_next_node_as_none() {
        let mut heap = Heap::with_capacity(10);
        let ptr = heap.allocate(String::new());
        assert_eq!(heap.entries[ptr.entry_index].header.next_node, None);
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
    }


    #[test]
    fn changing_data_through_gc_ptr_changes_data_in_heap_entry() {
        let mut heap = Heap::with_capacity(10);
        let mut ptr1 = heap.allocate(Vec::new());
        unsafe { ptr1.data.as_mut().push(RuntimeVal::NumberVal(2.0)) }
        let data_ref = *(&heap.entries[ptr1.entry_index]
            .data
            .as_ref()
            .unwrap()
            .as_ptr());
        let data_ref = data_ref as *const Vec<RuntimeVal>;
        let data_ref = unsafe { data_ref.as_ref().unwrap() };
        let heap_entry_val = match data_ref[0] {
            RuntimeVal::NumberVal(x) => x,
            _ => panic!("that should not happen"),
        };
        assert_eq!(data_ref.len(), 1);
        assert_eq!(heap_entry_val, 2.0)
    }

    #[test]
    fn cloned_gc_pointers_point_to_the_same_data() {
        let mut heap = Heap::with_capacity(10);
        let ptr1 = heap.allocate(String::new());
        let ptr2 = ptr1.clone();
        assert_eq!(ptr1.entry_index, ptr2.entry_index);
        assert_eq!(ptr1.data.as_ptr(), ptr2.data.as_ptr());
    }

    #[test]
    fn heap_size_at_lest_doubles_after_allocating_on_full_heap() {
        const INITIAL_HEAP_CAPACITY: usize = 10;
        let mut heap = Heap::with_capacity(INITIAL_HEAP_CAPACITY);
        for _ in 0..=INITIAL_HEAP_CAPACITY {
            heap.allocate(String::new());
        }
        assert!(heap.entries.len() >= INITIAL_HEAP_CAPACITY * 2);
    }
    #[test]
    fn full_heap_after_growing_has_vacant_entries() {
        // we need to have at least capacity 2 for this test to work
        // as capacity 1 heap could frow to 2 in allocation and immediately
        // take the only vacant entry, or it could grow more as `reserve` on `Vec`
        // can give us more if it wants to.
        let mut heap = Heap::with_capacity(2);
        heap.allocate(String::new());
        heap.allocate(String::new());
        heap.allocate(String::new());
        assert_eq!(heap.first_vacant.unwrap(), heap.entries.len() - 2);
    }

    #[test]
    fn full_heap_after_growing_does_not_invalidate_pointers() {
        let mut heap = Heap::with_capacity(1);
        let mut ptr = heap.allocate(Vec::new());
        heap.allocate(String::new());
        unsafe { ptr.data.as_mut().push(RuntimeVal::NumberVal(10.0)) };
        let data_ref = *(&heap.entries[ptr.entry_index]
            .data
            .as_ref()
            .unwrap()
            .as_ptr());
        let data_ref = data_ref as *const Vec<RuntimeVal>;
        let data_ref = unsafe { data_ref.as_ref().unwrap() };
        let heap_entry_val = match data_ref[0] {
            RuntimeVal::NumberVal(x) => x,
            _ => panic!("that should not happen"),
        };
        assert_eq!(data_ref.len(), 1);
        assert_eq!(heap_entry_val, 10.0);
    }

    #[test]
    fn heap_allocation_preserves_data() {
        let mut heap = Heap::new();
        let ptr = heap.allocate("Hello".to_string());
        assert_eq!(
            unsafe { ptr.data.as_ref() },
            "Hello"
        )
    }
}
