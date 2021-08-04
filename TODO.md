# todo

1. Functional tests
2. Documentation
3. Fix parsing as it panics when there is mismatched parenthesis
4. Using true lists instead of vectors
5. Should we go for interior mutability ? (will true lists even work without it? Cons has a value and pointer to another cons we need do deref the pointer clone pointer to rest deref it ---- yeah they will work)
6. Dereferencing Weak values is actually not safe at all even though it is wrapped in safe interface. We should do something about it so it is safe or marked as unsafe. I mean in debug build we check if the pointer is safe to dereference before we dereference it and technically we do not store not tracked weakpointers so any read after free should not happen but the function should probably be marked as unsafe.
7. Some ql stuff:
   1. argument dependant lookup call notation
   2. Pattern matching
8. More special forms (do we even have condition?)
   1. macros

      1. When and so on
   2. condition
9. Rest arguments in functions
10. Key-word args?
11. Errors
12. Add quasiqote list unpacking
13. Gen-sym std-lib
14. Stack vm (does not seem so bad but we need a compiler actually)
15. Namespacing
16. Objects?
17. Iterators and coroutines would be so coool
