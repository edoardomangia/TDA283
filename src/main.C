#include <iostream>
#include <fstream>
#include <exception>
#include "Absyn.H"
#include "Parser.H"
#include "TypeChecker.H"
#include "CodeGen.H"

int main(int argc, char **argv)
{
    // if (argc < 2) {
    //     std::cerr << "usage: " << argv[0] << " <source.jl>\n";
    //     return 1;
    // }

    try {
        // 1) parse
        FILE *in = std::fopen(argv[1], "r");
        if (!in) throw std::runtime_error(std::string("Cannot open input file: ") + argv[1]);
        Prog *p = pProg(in);                    // now matches pProg(FILE*)
        std::fclose(in);

        auto *prog = dynamic_cast<Program*>(p);
        if (!prog)
            throw std::runtime_error("Parser produced wrong root node");

        // 2) type-check
        TypeChecker tc;
        tc.checkProgram(prog);

        // 3) code-generation (to stdout)
        CodeGen cg(std::cout);
        cg.generate(prog);

        delete prog;

        // 4) signal success to the test harness
        std::cerr << "OK";
        return 0;
    }
    catch (const std::exception &e) {
        // first line MUST be ERROR
        std::cerr << "ERROR"
                  << e.what() << "\n";
        return 1;
    }
}
