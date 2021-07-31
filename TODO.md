# todo

1. Functional tests
2. Documentation
3. Fix parsing as it panics when there is mismatched parenthesis
4. Using true lists instead of vectors
5. Should we go for interior mutability ? (will true lists even work without it? Cons has a value and pointer to another cons we need do deref the pointer clone pointer to rest deref it ---- yeah they will work)
6. Ditch SymbolTable builder and provide two way mapping in vm by default
7. Put eny native type you want:
   1. Root\<dyn Dispatchable\>?
   2. obj.dispatch(op: string, args: Vec\<RootedVal\> -> RootedVal
   3. We have a problem with this protocol. dispatch needs access to vm to make allocations and we need to dereference it mutably to call anything on it so we have double mutable borrow. We could try to fix this with interior mutablity for the heap or something?
8. Dereferencing Weak values is actually not safe at all even though it is wrapped in safe interface. We should do something about it so it is safe or marked as unsafe.
9. Some ql stuff:
   1. argument dependant lookup call notation
   2. Pattern matching
10. More special forms (do we even have condition?)
    1. macros
       1. If
       2. When and so on
    2. let
    3. condition
11. Rest arguments in functions
12. Key-word args?
13. Errors
14. Gen-sym std-lib
15. Stack vm (does not seem so bad but we need a compiler actually)
16. Namespacing
17. Objects?
18. Iterators and coroutines would be so coool
