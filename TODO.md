# todo

1. Functional tests
2. Using true lists instead of vectors
3. Should we go for interior mutability ? (will true lists even work without it? Cons has a value and pointer to another cons we need do deref the pointer clone pointer to rest deref it ---- yeah they will work)
4. Put eny native type you want:
   1. Root\<dyn Dispatchable\>?
   2. obj.dispatch(op: string, args: Vec\<RootedVal\> -> RootedVal
   3. We have a problem with this protocol. dispatch needs access to vm to make allocations and we need to dereference it mutably to call anything on it so we have double mutable borrow. We could try to fix this with interior mutablity for the heap or something?
5. Some ql stuff:
   1. argument dependant lookup call notation
   2. Pattern matching
6. More special forms (do we even have condition?)
   1. macros
      1. If
      2. When and so on
   2. let
   3. condition
7. Rest arguments in functions
8. Key-word args?
9. Errors
10. Gen-sym std-lib
11. Stack vm (does not seem so bad but we need a compiler actually)
12. Namespacing
13. Objects?
14. Iterators and coroutines would be so coool
