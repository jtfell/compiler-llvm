compiler-llvm
------------------------

A toy project to experiment with the LLVM API.

Install
-------

Check that your installed LLVM version is precisely 5.0.

```bash
$ llvm-config --version
5.0
```

To build using stack:

```bash
$ stack build
$ stack exec main
```

Acknowledgements
----------------

Used https://github.com/llvm-hs/llvm-hs-kaleidoscope as a starting point.
