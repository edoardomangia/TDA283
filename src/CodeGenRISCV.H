// CodeGenRISCV.H
#pragma once

#include <iosfwd>
#include "Absyn.H"

void generateRISCV(Program *prog, std::ostream &out);
