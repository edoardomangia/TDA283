// CodeGenRISCV.H
#pragma once

#include <iosfwd>
#include "Absyn.H"

// RISC-V code generation entry point.
//
// The AST is expected to have passed parsing and type checking before this
// function is called. This backend lowers that validated Javalette program to
// textual RISC-V assembly and writes the assembly to the supplied output stream.
void generateRISCV(Program *prog, std::ostream &out);
