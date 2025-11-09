#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <exception>

#include "Absyn.H"
#include "Parser.H"
#include "ParserError.H"
#include "Printer.H"
#include "TypeChecker.H"

int main(int argc, char** argv)
{
    if (argc > 2) {
        std::cerr << "ERROR\n";
        std::cerr << "Usage: " << argv[0] << " [source-file]\n";
        return 1;
    }

    const char* filename = nullptr;
    FILE* input = nullptr;

    if (argc == 2) {
        filename = argv[1];
        input = std::fopen(filename, "r");
        if (!input) {
            std::perror(filename);
            return 1;
        }
    } else {
        // No filename given: read from stdin
        input = stdin;
    }

    Prog* prog = nullptr;

    try {
        // Parse the program
        prog = pProg(input);

        if (input && input != stdin) {
            std::fclose(input);
            input = nullptr;
        }

        // (Optional) Pretty-print the AST for debugging:
        // PrintAbsyn printer;
        // char* pretty = printer.print(prog);
        // std::cout << pretty << std::endl;

        // Type check
        TypeChecker tc;
        tc.checkProgram(prog);

        // If we reach here, everything is fine
        std::cerr << "OK\n";

        delete prog;
        return 0;
    }
    catch (parse_error& e) {
        if (input && input != stdin) {
            std::fclose(input);
        }
        if (prog) {
            delete prog;
        }
        std::cerr << "ERROR\n";
        std::cerr << "Parse error at line " << e.getLine()
                  << ": " << e.what() << "\n";
        return 1;
    }
    catch (const TypeError& e) {
        if (input && input != stdin) {
            std::fclose(input);
        }
        if (prog) {
            delete prog;
        }
        std::cerr << "ERROR\n";
        std::cerr << "Type error: " << e.what() << "\n";
        return 1;
    }
    catch (const std::exception& e) {
        if (input && input != stdin) {
            std::fclose(input);
        }
        if (prog) {
            delete prog;
        }
        std::cerr << "ERROR\n";
        std::cerr << "Internal error: " << e.what() << "\n";
        return 1;
    }
}

