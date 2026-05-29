# Javalette Compiler

This repository contains a compiler for Javalette, a small imperative C-like
language.

It is intended as an educational side project. The choices made for its
architecture, behavior, and implemented features reflect what I wanted to learn
from building it.

Some of its features are:

- a BNFC/Flex/Bison-generated frontend;
- semantic type checking;
- LLVM IR output through `jlc`;
- RV64 RISC-V output through `jlc_riscv`.

More thorough documentation is available in [`doc/TDA283.md`](doc/TDA283.md).

## Usage

Build the LLVM compiler:

```sh
make
```

This creates `./jlc`. If `riscv64-unknown-linux-gnu-gcc` is available, the
build also creates `./jlc_riscv` and `lib/runtime-riscv.o`.

Compile a Javalette program to LLVM IR:

```sh
./jlc program.jl > program.ll
```

The compiler can also read from standard input:

```sh
./jlc < program.jl > program.ll
```

## About Javalette

This implementation supports the base language plus array extensions, including:

- scalar types `int`, `double`, `boolean`, and `void`;
- functions with scalar or array parameters and return values;
- declarations, assignments, blocks, conditionals, loops, and returns;
- arithmetic, relational, boolean, and unary expressions;
- one-dimensional and multidimensional arrays;
- array creation, indexing, indexed assignment, `.length`, and foreach loops.
