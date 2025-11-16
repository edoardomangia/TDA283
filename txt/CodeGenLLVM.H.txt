// CodeGenLLVM.H
#pragma once

#include <iosfwd>
#include "Absyn.H"

// Generate LLVM IR for the given (type-checked) program onto 'out'.
void generateLLVM(Program* prog, std::ostream& out);

