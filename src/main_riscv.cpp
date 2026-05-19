#include <cstdio>
#include <cstdlib>
#include <exception>
#include <iostream>

#include "Absyn.H"
#include "CodeGenRISCV.H"
#include "Parser.H"
#include "ParserError.H"
#include "Printer.H"
#include "TypeChecker.H"

int main(int argc, char **argv) {
  if (argc > 2) {
    std::cerr << "ERROR\n";
    std::cerr << "Usage: " << argv[0] << " [source-file]\n";
    return 1;
  }

  const char *filename = nullptr;
  FILE *input = nullptr;

  if (argc == 2) {
    filename = argv[1];
    input = std::fopen(filename, "r");
    if (!input) {
      std::perror(filename);
      return 1;
    }
  } else {
    input = stdin;
  }

  Prog *prog = nullptr;

  try {
    prog = pProg(input);

    if (input && input != stdin) {
      std::fclose(input);
      input = nullptr;
    }

    if (!prog) {
      std::cerr << "ERROR\n";
      return 1;
    }

    TypeChecker tc;
    tc.checkProgram(prog);

    Program *ast = dynamic_cast<Program *>(prog);
    if (!ast) {
      std::cerr << "ERROR\n";
      delete prog;
      return 1;
    }

    generateRISCV(ast, std::cout);
    std::cerr << "OK\n";

    delete prog;
    return 0;
  } catch (parse_error &e) {
    if (input && input != stdin) {
      std::fclose(input);
    }
    delete prog;
    std::cerr << "ERROR\n";
    std::cerr << e.what() << "\n";
    return 1;
  } catch (const TypeError &e) {
    if (input && input != stdin) {
      std::fclose(input);
    }
    delete prog;
    std::cerr << "ERROR\n";
    std::cerr << "Type error: " << e.what() << "\n";
    return 1;
  } catch (const std::exception &e) {
    if (input && input != stdin) {
      std::fclose(input);
    }
    delete prog;
    std::cerr << "ERROR\n";
    std::cerr << "Internal error: " << e.what() << "\n";
    return 1;
  }
}
