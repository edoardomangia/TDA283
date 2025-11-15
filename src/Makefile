## Makefile

CC=g++ -g
CCFLAGS= -std=c++17 -W -Wall -Wsign-conversion -Wno-unused-parameter -Wno-unused-function -Wno-unneeded-internal-declaration

FLEX=flex
FLEX_OPTS=-Pjavalette_

BISON=bison
BISON_OPTS=-t -pjavalette_

OBJS=Absyn.o Buffer.o Lexer.o Parser.o Printer.o Skeleton.o TypeChecker.o CodeGenLLVM.o
 
.PHONY : clean distclean

all : jlc

jlc : ${OBJS} main.o
	${CC} ${OBJS} main.o -o jlc

Absyn.o : Absyn.C Absyn.H
	${CC} ${CCFLAGS} -c Absyn.C

Buffer.o : Buffer.C Buffer.H
	${CC} ${CCFLAGS} -c Buffer.C

Lexer.C : Javalette.l
	${FLEX} ${FLEX_OPTS} -oLexer.C Javalette.l

Parser.C Bison.H : Javalette.y
	${BISON} ${BISON_OPTS} Javalette.y -o Parser.C

Lexer.o : CCFLAGS+= -Wno-sign-conversion

Lexer.o : Lexer.C Bison.H
	${CC} ${CCFLAGS} -c Lexer.C

Parser.o : Parser.C Absyn.H Bison.H
	${CC} ${CCFLAGS} -c Parser.C

Printer.o : Printer.C Printer.H Absyn.H
	${CC} ${CCFLAGS} -c Printer.C

Skeleton.o : Skeleton.C Skeleton.H Absyn.H
	${CC} ${CCFLAGS} -Wno-unused-parameter -c Skeleton.C

TypeChecker.o : TypeChecker.C TypeChecker.H Absyn.H Skeleton.H
	${CC} ${CCFLAGS} -c TypeChecker.C 

CodeGenLLVM.o : CodeGenLLVM.cpp CodeGenLLVM.H Absyn.H
	${CC} ${CCFLAGS} -c CodeGenLLVM.cpp 

main.o : main.cpp
	${CC} ${CCFLAGS} -c main.cpp 

# Test.o : Test.C Parser.H Printer.H Absyn.H
#	${CC} ${CCFLAGS} -c Test.C

test:
	./test.sh

clean :
	rm -f *.o jlc Javalette.aux Javalette.log Javalette.pdf Javalette.dvi Javalette.ps Javalette

distclean : clean
	rm -f Absyn.C Absyn.H Buffer.C Buffer.H Test.C Bison.H Parser.C Parser.H ParserError.H Javalette.y Lexer.C Javalette.l Skeleton.C Skeleton.H Printer.C Printer.H Makefile Javalette.tex

