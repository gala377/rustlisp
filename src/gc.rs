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

const INITIAL_HEAP_SIZE: usize = 256;
const MEMORY_LIMIT: usize = 2050;

fn get_preinitialized_heap_storage() -> Vec<HeapEntry> {
    let mut heap = Vec::with_capacity(INITIAL_HEAP_SIZE);
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
    pub fn new() -> Self {
        Self {
            entries: get_preinitialized_heap_storage(),
            first_taken: None,
            first_vacant: Some(INITIAL_HEAP_SIZE - 1),
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
