# todo

1. Documentation
2. Write a proper expander

   1. Expand let to lambda call
3. Make read/write and eval a library functions
4. Using true lists instead of vectors
5. How do
6. Should we go for interior mutability ? (will true lists even work without it? Cons has a value and pointer to another cons we need do deref the pointer clone pointer to rest deref it ---- yeah they will work)
7. Dereferencing Weak values is actually not safe at all even though it is wrapped in safe interface. We should do something about it so it is safe or marked as unsafe. I mean in debug build we check if the pointer is safe to dereference before we dereference it and technically we do not store not tracked weakpointers so any read after free should not happen but the function should probably be marked as unsafe.
8. Unquote and unquote-splicing evaluation only checks for the list of len 2 so incorrect forms like (unquote 1 2 3) will panic on really confusing error "unquote not in quasiquote context"
9. More functions in vtable, maybe just like python

   1. str?
   2. repr
   3. add
   4. sub
   5. mul
   6. and dispatch as last resort
10. Some ql stuff:

    1. argument dependant lookup call notation
    2. Pattern matching
11. More special forms (do we even have condition?)

    1. condition
12. Eval macro
13. Key-word args?
14. Optional args in functions
15. True module system maybe like in ocaml?
16. Errors
17. Gen-sym std-lib
18. Stack vm (does not seem so bad but we need a compiler actually)
19. Namespacing
20. Objects?
21. Iterators and coroutines would be so coool
