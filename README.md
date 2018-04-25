# Corrupted Memory Example

This repository is a copy of my `packed` library. However, I have removed
as many things as possible to make a minimal reproducible case of memory
corruption that causes the GHC runtime to crash during the garbage collection.

Discussion is happening on [trac ticket 15038](https://ghc.haskell.org/trac/ghc/ticket/15038).

# Running

```
$ make deps TEST_HC=ghc-stage2
$ make TEST_HC=ghc-stage2
$ ./Main
```
