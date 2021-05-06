use std::ptr;

use crate::runtime::{self, RuntimeVal};


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

impl Drop for HeapEntry {
    fn drop(&mut self) {
        match &mut self.data {
            None => (),
            Some(ptr) => match &self.header.tag {
                TypeTag::Lambda => unsafe {
                    println!("Dropping lambda");
                    std::ptr::drop_in_place(ptr.as_ptr() as *mut runtime::Lambda);
                }
                TypeTag::List => unsafe {
                    println!("Dropping list");
                    std::ptr::drop_in_place(ptr.as_ptr() as *mut Vec<RuntimeVal>);
                }
                TypeTag::RuntimeFunc => unsafe {
                    println!("Dropping function");
                    std::ptr::drop_in_place(ptr.as_ptr() as *mut runtime::RuntimeFunc);
                }
                TypeTag::String => unsafe {
                    println!("Dropping string");
                    std::ptr::drop_in_place(ptr.as_ptr() as *mut std::string::String);
                }
                TypeTag::None => (),
            }
        }
    }
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

    pub fn as_boxed_bytes<T>(val: T) -> *mut T {
        let ptr = Box::new(val);
        Box::into_raw(ptr)
    }
}

// #[cfg(test)]
// mod tests {

//     use super::*;
//     use crate::runtime;

//     #[test]
//     fn empty_heap_has_nonzero_capacity() {
//         let heap = Heap::new();
//         assert_ne!(heap.entries.len(), 0);
//     }

//     #[test]
//     fn new_heap_has_correct_capacity() {
//         let cap = 10;
//         let heap = Heap::with_capacity(cap);
//         assert_eq!(heap.entries.len(), cap);
//     }

//     #[test]
//     #[should_panic]
//     fn heap_with_capacity_zero_panics() {
//         Heap::with_capacity(0);
//     }

//     #[test]
//     fn new_heap_has_vacant_entry_set() {
//         let heap = Heap::with_capacity(10);
//         assert!(heap.first_vacant.is_some());
//     }

//     #[test]
//     fn new_heap_has_not_set_taken_entry() {
//         let heap = Heap::with_capacity(10);
//         assert!(heap.first_taken.is_none());
//     }

//     #[test]
//     fn new_heap_has_no_taken_entries() {
//         let heap = Heap::with_capacity(10);
//         assert!(heap.entries.iter().all(|e| e.data.is_none()))
//     }

//     #[test]
//     fn new_heap_vacant_entries_are_linked() {
//         let cap = 10;
//         let heap = Heap::with_capacity(cap);
//         let mut curr = heap.first_vacant;
//         let mut vacant_entries = Vec::new();
//         while let Some(i) = curr {
//             vacant_entries.push(i);
//             curr = heap.entries[i].header.next_node;
//         }
//         vacant_entries.dedup();
//         assert_eq!(vacant_entries.len(), cap);
//     }

//     // #[test]
//     // fn allocating_entry_sets_first_taken_to_first_vacant() {
//     //     let mut heap = Heap::new();
//     //     let first_vacant = heap.first_vacant.clone();
//     //     heap.allocate(runtime::List::new());
//     //     assert_eq!(first_vacant, heap.first_taken);
//     // }

//     // #[test]
//     // fn allocating_last_vacant_entry_in_heap_sets_vacant_entry_to_none() {
//     //     let mut heap = Heap::new();
//     //     heap.allocate(runtime::List::new());
//     //     assert_eq!(heap.first_vacant, None);
//     // }

//     // #[test]
//     // fn allocating_with_multiple_vacant_entries_moves_vacant_entry_to_the_next() {
//     //     let mut heap = Heap::with_capacity(10);
//     //     let next_vacant = heap.entries[heap.first_vacant.unwrap()].header.next_node;
//     //     heap.allocate(runtime::List::new());
//     //     assert_eq!(heap.first_vacant, next_vacant);
//     // }

//     // #[test]
//     // fn first_allocated_heap_entry_has_next_node_as_none() {
//     //     let mut heap = Heap::with_capacity(10);
//     //     let ptr = heap.allocate(runtime::List::new());
//     //     assert_eq!(heap.entries[ptr.entry_index].header.next_node, None);
//     // }



//     // TODO: expected Box<[u8]> instead of Box<()>
//     // #[test]
//     // fn second_allocated_entry_does_not_change_the_data_of_the_first_entry() {
//     //     let mut heap = Heap::with_capacity(10);
//     //     let mut val1 = runtime::List::new();
//     //     val1.data.push(RuntimeVal::NumberVal(2.0));

//     //     let ptr1 = heap.allocate(val1);

//     //     let entry1_data = (&heap.entries[ptr1.entry_index])
//     //         .data
//     //         .as_ref()
//     //         .unwrap()
//     //         .to_vec();
//     //     let entry1_header = (&heap.entries[ptr1.entry_index]).header.clone();

//     //     let ptr2 = heap.allocate(runtime::List::new());

//     //     let entry2_data = (&heap.entries[ptr2.entry_index])
//     //         .data
//     //         .as_ref()
//     //         .unwrap()
//     //         .to_vec();
//     //     let entry2_header = (&heap.entries[ptr2.entry_index]).header.clone();

//     //     let entry1_new_data = (&heap.entries[ptr1.entry_index])
//     //         .data
//     //         .as_ref()
//     //         .unwrap()
//     //         .to_vec();
//     //     let entry1_new_header = (&heap.entries[ptr1.entry_index]).header.clone();

//     //     assert_eq!(entry1_data, entry1_new_data);
//     //     assert_eq!(entry1_header, entry1_new_header);
//     //     assert_ne!(entry2_data, entry1_data);
//     //     assert_ne!(entry2_header, entry1_header);
//     // }

//     #[test]
//     fn second_allocated_entry_points_to_the_first_entry() {
//         let mut heap = Heap::with_capacity(10);
//         let ptr1 = heap.allocate(runtime::List::new());
//         let ptr2 = heap.allocate(runtime::List::new());
//         assert_eq!(
//             heap.entries[ptr2.entry_index].header.next_node.unwrap(),
//             ptr1.entry_index
//         );
//     }


//     // TODO: expected Box<[u8]> instead of Box<()>
//     // #[test]
//     // fn changing_data_through_gc_ptr_changes_data_in_heap_entry() {
//     //     let mut heap = Heap::with_capacity(10);
//     //     let mut ptr1 = heap.allocate(runtime::List::new());
//     //     unsafe { ptr1.data.as_mut().data.push(RuntimeVal::NumberVal(2.0)) }
//     //     let data_ref = *(&heap.entries[ptr1.entry_index]
//     //         .data
//     //         .as_ref()
//     //         .unwrap()
//     //         .as_ptr());
//     //     let data_ref = data_ref as *const runtime::List;
//     //     let data_ref = unsafe { data_ref.as_ref().unwrap() };
//     //     let heap_entry_val = match data_ref.data[0] {
//     //         RuntimeVal::NumberVal(x) => x,
//     //         _ => panic!("that should not happen"),
//     //     };
//     //     assert_eq!(data_ref.data.len(), 1);
//     //     assert_eq!(heap_entry_val, 2.0)
//     // }

//     #[test]
//     fn cloned_gc_pointers_point_to_the_same_data() {
//         let mut heap = Heap::with_capacity(10);
//         let ptr1 = heap.allocate(runtime::List::new());
//         let ptr2 = ptr1.clone();
//         assert_eq!(ptr1.entry_index, ptr2.entry_index);
//         assert_eq!(ptr1.data.as_ptr(), ptr2.data.as_ptr());
//     }

//     #[test]
//     fn heap_size_at_lest_doubles_after_allocating_on_full_heap() {
//         const INITIAL_HEAP_CAPACITY: usize = 10;
//         let mut heap = Heap::with_capacity(INITIAL_HEAP_CAPACITY);
//         for _ in 0..=INITIAL_HEAP_CAPACITY {
//             heap.allocate(runtime::List::new());
//         }
//         assert!(heap.entries.len() >= INITIAL_HEAP_CAPACITY * 2);
//     }
//     #[test]
//     fn full_heap_after_growing_has_vacant_entries() {
//         // we need to have at least capacity 2 for this test to work
//         // as capacity 1 heap could frow to 2 in allocation and immediately
//         // take the only vacant entry, or it could grow more as `reserve` on `Vec`
//         // can give us more if it wants to.
//         let mut heap = Heap::with_capacity(2);
//         heap.allocate(runtime::List::new());
//         heap.allocate(runtime::List::new());
//         heap.allocate(runtime::List::new());
//         assert_eq!(heap.first_vacant.unwrap(), heap.entries.len() - 2);
//     }

//     // TODO: expected Box<[u8]> instead of Box<()>
//     // #[test]
//     // fn full_heap_after_growing_does_not_invalidate_pointers() {
//     //     let mut heap = Heap::with_capacity(1);
//     //     let mut ptr = heap.allocate(runtime::List::new());
//     //     heap.allocate(runtime::String::new());
//     //     unsafe { ptr.data.as_mut().data.push(RuntimeVal::NumberVal(10.0)) };
//     //     let data_ref = *(&heap.entries[ptr.entry_index]
//     //         .data
//     //         .as_ref()
//     //         .unwrap()
//     //         .as_ptr());
//     //     let data_ref = data_ref as *const runtime::List;
//     //     let data_ref = unsafe { data_ref.as_ref().unwrap() };
//     //     let heap_entry_val = match data_ref.data[0] {
//     //         RuntimeVal::NumberVal(x) => x,
//     //         _ => panic!("that should not happen"),
//     //     };
//     //     assert_eq!(data_ref.data.len(), 1);
//     //     assert_eq!(heap_entry_val, 10.0);
//     // }

//     #[test]
//     fn heap_allocation_preserves_data() {
//         let mut heap = Heap::new();
//         let ptr = heap.allocate("Hello".to_string());
//         assert_eq!(
//             unsafe { ptr.data.as_ref() },
//             "Hello"
//         )
//     }
// }
