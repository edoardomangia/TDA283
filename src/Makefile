CC      = g++
CXXFLAGS = -g -std=c++17 -Wall -Wextra -Wno-unused-parameter -Wno-unused-function

FLEX    = flex
FLEX_OPTS = -Pjavalette_

BISON   = bison
BISON_OPTS = -t -pjavalette_

# Objects for your real compiler:
OBJS = Absyn.o Buffer.o Lexer.o Parser.o Printer.o Skeleton.o TypeChecker.o CodeGenLLVM.o main.o

.PHONY: all clean distclean

# Build your compiler
all: jlc

jlc: $(OBJS)
	@echo "Linking jlc..."
	$(CC) $(CXXFLAGS) $(OBJS) -o jlc

# --- BNFC-generated stuff ---

Absyn.o: Absyn.C Absyn.H
	$(CC) $(CXXFLAGS) -c Absyn.C

Buffer.o: Buffer.C Buffer.H
	$(CC) $(CXXFLAGS) -c Buffer.C

Lexer.C: Javalette.l
	$(FLEX) $(FLEX_OPTS) -oLexer.C Javalette.l

Parser.C Bison.H: Javalette.y
	$(BISON) $(BISON_OPTS) Javalette.y -o Parser.C

Lexer.o: Lexer.C Bison.H
	$(CC) $(CXXFLAGS) -Wno-sign-conversion -c Lexer.C

Parser.o: Parser.C Absyn.H Bison.H
	$(CC) $(CXXFLAGS) -c Parser.C

Printer.o: Printer.C Printer.H Absyn.H
	$(CC) $(CXXFLAGS) -c Printer.C

# --- Your own files ---

TypeChecker.o: TypeChecker.C TypeChecker.H Absyn.H
	$(CC) $(CXXFLAGS) -c TypeChecker.C

CodeGenLLVM.o: CodeGenLLVM.cpp CodeGenLLVM.H Absyn.H
	$(CC) $(CXXFLAGS) -c CodeGenLLVM.cpp

main.o: main.cpp Absyn.H Parser.H TypeChecker.H CodeGenLLVM.H
	$(CC) $(CXXFLAGS) -c main.cpp

clean:
	rm -f *.o jlc TestJavalette Javalette.aux Javalette.log Javalette.pdf \
	      Javalette.dvi Javalette.ps Javalette

distclean: clean
	rm -f Absyn.C Absyn.H Buffer.C Buffer.H Test.C Bison.H Parser.C Parser.H \
	      ParserError.H Javalette.y Lexer.C Javalette.l Skeleton.C Skeleton.H \
	      Printer.C Printer.H Makefile Javalette.tex

