// CodeGenLLVM.H
#pragma once

#include <iosfwd>
#include "Absyn.H"

// LLVM code generation entry point.
//
// The parser and type checker build and validate the BNFC AST before this
// function is called. This backend therefore assumes that names resolve,
// expressions have legal types, and all function calls match their signatures.
// Its job is to lower the type-checked Javalette program to textual LLVM IR and
// write that IR to the supplied output stream.
void generateLLVM(Program* prog, std::ostream& out);
