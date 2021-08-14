# todo

1. Functional tests
2. Documentation
3. Make read/write and eval a library functions
4. Using true lists instead of vectors
5. Should we go for interior mutability ? (will true lists even work without it? Cons has a value and pointer to another cons we need do deref the pointer clone pointer to rest deref it ---- yeah they will work)
6. Dereferencing Weak values is actually not safe at all even though it is wrapped in safe interface. We should do something about it so it is safe or marked as unsafe. I mean in debug build we check if the pointer is safe to dereference before we dereference it and technically we do not store not tracked weakpointers so any read after free should not happen but the function should probably be marked as unsafe.
7. Unquote and unquote-splicing evaluation only checks for the list of len 2 so incorrect forms like (unquote 1 2 3) will panic on really confusing error "unquote not in quasiquote context"
8. More functions in vtable, maybe just like python
   1. str?
   2. repr
   3. add
   4. sub
   5. mul
   6. and dispatch as last resort
9. Some ql stuff:
   1. argument dependant lookup call notation
   2. Pattern matching
10. More special forms (do we even have condition?)
    1. condition
11. Rest arguments in functions
12. Key-word args?
13. Optional args in functions
14. True module system maybe like in ocaml?
15. Errors
16. Gen-sym std-lib
17. Stack vm (does not seem so bad but we need a compiler actually)
18. Namespacing
19. Objects?
20. Iterators and coroutines would be so coool
