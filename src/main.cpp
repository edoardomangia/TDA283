#include <cstdio>
#include <cstdlib>
#include <iostream>

#include "Absyn.H"
#include "Parser.H"
#include "ParserError.H"
#include "Printer.H"

int main(int argc, char** argv)
{
    if (argc > 2) {
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

    try {
        // Parse the program
        Prog* prog = pProg(input);

        if (input != stdin) {
            std::fclose(input);
        }

        // Debug: pretty-print the parsed AST back out
        PrintAbsyn printer;
        char* pretty = printer.print(prog);
        std::cout << pretty << std::endl;

        delete prog;   // clean up AST
        return 0;
    }
    catch (const parse_error& e) {
        if (input && input != stdin) {
            std::fclose(input);
        }
        std::cerr << "Parse error at line " << e.getLine()
                  << ": " << e.what() << "\n";
        return 1;
    }
    catch (const std::exception& e) {
        if (input && input != stdin) {
            std::fclose(input);
        }
        std::cerr << "Error: " << e.what() << "\n";
        return 1;
    }
}

