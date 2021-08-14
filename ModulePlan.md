1. Each module is a file
2. load loads from a fle/ whatever
3. import loads file as a module so
   1. (import std/collections) will import std/collections.rlp file
   2. It will evaluate it and put into modules in vm but won't add its bindings to current env
4. When reader sees symbol std/collections/list? it changes it into
   1. (lookup-module-item "std/collections.rlp" (quote list?))
   2. this functions look into this modules global env, find the item and returns it.
5. that easy but what now about native bindings?
