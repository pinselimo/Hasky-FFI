# Pythas-FFI [![Build](https://img.shields.io/travis/pinselimo/Pythas-FFI.svg)](https://travis-ci.org/pinselimo/Pythas-FFI)

A wrapper for creating FFI modules out of Haskell source files. This project is part of the development of the Python library ```Pythas```.

## Pythas

As the part of ```Pythas``` implemented in Haskell grew, it has been moved to its own repo. The relationship in between the both stays strongly connected.
```Pythas``` relies heavily on the types declared in ```Foreign.Pythas.Array```, ```Foreign.Pythas.String``` and ```Foreign.Pythas.Tuples```.
If you are looking for the Python equivalents, take a look at [```hasky.types.py```](https://github.com/pinselimo/Pythas/blob/master/hasky/types.py)

## Flavours

There two flavours that Pythas-FFI comes in. You can install it as an executable or as a ```Haskell``` library. That's configurable via ```cabal-install```.

### Executable

**Not yet implemented!** The executable can be used to create Haskell FFI export modules for any fitting Haskell module from the command line.

### Library

As a library Pythas-FFI supports live FFI file generation. Thus it is useful for libraries connecting Haskell to foreign languages.

## License

Just as the Python part, this package is licensed under the **LGPLv3** license. Find details of the license in the COPYING and COPYING.LESSER files.

>(c) 2020 Simon Plakolb

