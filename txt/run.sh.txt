#!/usr/bin/env bash

./jlc hello.jl > hello.ll
llvm-as hello.ll -o hello.bc
llvm-link hello.bc ../lib/runtime.bc -o hello-linked.bc
lli hello-linked.bc
