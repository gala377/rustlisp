use std::ptr;

use crate::runtime;

pub enum TypeTag {
    Lambda,
    List,
    String,
    None,
}

pub struct Gc<T> {
    pub data: ptr::NonNull<T>,
    pub entry_index: usize,
}

impl<T> Clone for Gc<T> {
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

impl Allocable for runtime::Lambda {
    fn tag() -> TypeTag {
        TypeTag::Lambda
    }
}

impl Allocable for runtime::String {
    fn tag() -> TypeTag {
        TypeTag::String
    }
}

impl Allocable for runtime::List {
    fn tag() -> TypeTag {
        TypeTag::List
    }
}

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
    pub data: Option<Box<[u8]>>,
    pub header: Header,
}

pub struct Heap {
    pub entries: Vec<HeapEntry>,
    pub first_vacant: Option<usize>,
    pub first_taken: Option<usize>,
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
        }
    }

    pub fn allocate<T: Allocable>(&mut self, val: T) -> Gc<T> {
        let data = Self::as_boxed_bytes(val);
        let header = Header {
            marked: false,
            rooted: false,
            size: std::mem::size_of::<T>(),
            tag: T::tag(),
            next_node: self.first_taken,
        };
        let entry = HeapEntry {
            data: Some(data),
            header,
        };
        let entry_index = self.insert_entry(entry);
        let ptr = self.entries[entry_index]
            .data
            .as_mut()
            .unwrap()
            .as_mut_ptr() as *mut T;
        Gc {
            data: unsafe { ptr::NonNull::new_unchecked(ptr) },
            entry_index,
        }
    }

    pub fn free<T: Allocable>(&mut self, ptr: Gc<T>) {
        // todo: implement
        unimplemented!()
    }

    pub fn free_and_drop<T: Allocable>(&mut self, ptr: Gc<T>) {
        // todo: implement
        unimplemented!()
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
    }

    pub fn as_boxed_bytes<T>(val: T) -> Box<[u8]> {
        let ptr: *const T = &val;
        let slice =
            unsafe { std::slice::from_raw_parts(ptr as *const u8, std::mem::size_of::<T>()) };
        // todo: confirm it copies data
        let data: Box<[u8]> = slice.into();
        data
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
        heap.allocate(runtime::List::new());
        assert_eq!(first_vacant, heap.first_taken);
    }

    #[test]
    fn allocating_last_vacant_entry_in_heap_sets_vacant_entry_to_none() {
        let mut heap = Heap::new();
        heap.allocate(runtime::List::new());
        assert_eq!(heap.first_vacant, None);
    }

    #[test]
    fn allocating_with_multiple_vacant_entries_moves_vacant_entry_to_the_next() {
        let mut heap = Heap::with_capacity(10);
        let next_vacant = heap.entries[heap.first_vacant.unwrap()].header.next_node;
        heap.allocate(runtime::List::new());
        assert_eq!(heap.first_vacant, next_vacant);
    }

    #[test]
    fn first_allocated_heap_entry_has_next_node_as_none() {
        let mut heap = Heap::with_capacity(10);
        let ptr = heap.allocate(runtime::List::new());
        assert_eq!(heap.entries[ptr.entry_index].header.next_node, None);
    }

    // second_allocated_entry_doesnt_change_the_data_of_the_first_entry
    // second_allocated_entry_points_to_the_first_entry
    // changing_data_through_gc_ptr_changes_data_in_heap_entry
    // cloned_gc_pointers_point_to_the_same_data
    // cloned_gc_pointers_can_can_change_the_same_data
    // heap_size_doubles_after_allocating_on_full_heap
    // full_heap_after_growing_has_vacant_entries
    // full_heap_after_growing_has_all_data_copied
    // full_heap_after_growing_does_not_invalidate_pointers
    // freeing_data_of_an_entry_sets_its_index_as_first_vacant
    // freeing_data_sets_new_vacant_entry_next_node_as_previous_first_vacant
}
