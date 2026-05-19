CC = g++
CXXFLAGS = -g -std=c++17 -Wall -Wextra -Wno-unused-parameter -Wno-unused-function

FLEX = flex
FLEX_OPTS = -Pjavalette_

BISON = bison
BISON_OPTS = -t -pjavalette_

BNFC = ./src/bnfc
RISCV_GCC ?= riscv64-unknown-linux-gnu-gcc
HAVE_RISCV_GCC := $(shell command -v $(RISCV_GCC) 2>/dev/null)

SRC_DIR = src
COMMON_OBJS = \
	$(SRC_DIR)/Absyn.o \
	$(SRC_DIR)/Buffer.o \
	$(SRC_DIR)/Lexer.o \
	$(SRC_DIR)/Parser.o \
	$(SRC_DIR)/Printer.o \
	$(SRC_DIR)/Skeleton.o \
	$(SRC_DIR)/TypeChecker.o
LLVM_OBJS = $(COMMON_OBJS) $(SRC_DIR)/CodeGenLLVM.o $(SRC_DIR)/main.o
RISCV_OBJS = $(COMMON_OBJS) $(SRC_DIR)/CodeGenRISCV.o $(SRC_DIR)/main_riscv.o

ifeq ($(HAVE_RISCV_GCC),)
ALL_TARGETS = jlc
else
ALL_TARGETS = jlc jlc_riscv lib/runtime-riscv.o
endif

.PHONY: all clean distclean

all: $(ALL_TARGETS)

jlc: $(SRC_DIR)/jlc
	cp $(SRC_DIR)/jlc $@

jlc_riscv: $(SRC_DIR)/jlc_riscv
	cp $(SRC_DIR)/jlc_riscv $@

$(SRC_DIR)/jlc: $(LLVM_OBJS)
	@echo "Linking $(SRC_DIR)/jlc..."
	$(CC) $(CXXFLAGS) $(LLVM_OBJS) -o $@

$(SRC_DIR)/jlc_riscv: $(RISCV_OBJS)
	@echo "Linking $(SRC_DIR)/jlc_riscv..."
	$(CC) $(CXXFLAGS) $(RISCV_OBJS) -o $@

$(SRC_DIR)/Absyn.C $(SRC_DIR)/Absyn.H $(SRC_DIR)/Buffer.C $(SRC_DIR)/Buffer.H $(SRC_DIR)/Parser.H $(SRC_DIR)/ParserError.H $(SRC_DIR)/Printer.C $(SRC_DIR)/Printer.H $(SRC_DIR)/Skeleton.C $(SRC_DIR)/Skeleton.H $(SRC_DIR)/Test.C $(SRC_DIR)/Javalette.y $(SRC_DIR)/Javalette.l: $(SRC_DIR)/Javalette.cf
	$(BNFC) --cpp -o $(SRC_DIR) $<

$(SRC_DIR)/Absyn.o: $(SRC_DIR)/Absyn.C $(SRC_DIR)/Absyn.H
	$(CC) $(CXXFLAGS) -c $< -o $@

$(SRC_DIR)/Buffer.o: $(SRC_DIR)/Buffer.C $(SRC_DIR)/Buffer.H
	$(CC) $(CXXFLAGS) -c $< -o $@

$(SRC_DIR)/Lexer.C: $(SRC_DIR)/Javalette.l
	$(FLEX) $(FLEX_OPTS) -o$@ $<

$(SRC_DIR)/Parser.C $(SRC_DIR)/Bison.H: $(SRC_DIR)/Javalette.y
	$(BISON) $(BISON_OPTS) $< -o $(SRC_DIR)/Parser.C
	cp Bison.H $(SRC_DIR)/Bison.H

$(SRC_DIR)/Lexer.o: $(SRC_DIR)/Lexer.C $(SRC_DIR)/Bison.H
	$(CC) $(CXXFLAGS) -Wno-sign-conversion -c $< -o $@

$(SRC_DIR)/Parser.o: $(SRC_DIR)/Parser.C $(SRC_DIR)/Absyn.H $(SRC_DIR)/Bison.H
	$(CC) $(CXXFLAGS) -c $< -o $@

$(SRC_DIR)/Printer.o: $(SRC_DIR)/Printer.C $(SRC_DIR)/Printer.H $(SRC_DIR)/Absyn.H
	$(CC) $(CXXFLAGS) -c $< -o $@

$(SRC_DIR)/Skeleton.o: $(SRC_DIR)/Skeleton.C $(SRC_DIR)/Skeleton.H $(SRC_DIR)/Absyn.H
	$(CC) $(CXXFLAGS) -c $< -o $@

$(SRC_DIR)/TypeChecker.o: $(SRC_DIR)/TypeChecker.C $(SRC_DIR)/TypeChecker.H $(SRC_DIR)/Absyn.H
	$(CC) $(CXXFLAGS) -c $< -o $@

$(SRC_DIR)/CodeGenLLVM.o: $(SRC_DIR)/CodeGenLLVM.cpp $(SRC_DIR)/CodeGenLLVM.H $(SRC_DIR)/Absyn.H
	$(CC) $(CXXFLAGS) -c $< -o $@

$(SRC_DIR)/CodeGenRISCV.o: $(SRC_DIR)/CodeGenRISCV.cpp $(SRC_DIR)/CodeGenRISCV.H $(SRC_DIR)/Absyn.H
	$(CC) $(CXXFLAGS) -c $< -o $@

$(SRC_DIR)/main.o: $(SRC_DIR)/main.cpp $(SRC_DIR)/Absyn.H $(SRC_DIR)/Parser.H $(SRC_DIR)/TypeChecker.H $(SRC_DIR)/CodeGenLLVM.H
	$(CC) $(CXXFLAGS) -c $< -o $@

$(SRC_DIR)/main_riscv.o: $(SRC_DIR)/main_riscv.cpp $(SRC_DIR)/Absyn.H $(SRC_DIR)/Parser.H $(SRC_DIR)/TypeChecker.H $(SRC_DIR)/CodeGenRISCV.H
	$(CC) $(CXXFLAGS) -c $< -o $@

lib/runtime-riscv.o: lib/runtime-riscv.s
ifeq ($(HAVE_RISCV_GCC),)
	@echo "riscv64 cross-compiler not found: cannot build $@" >&2
	@false
else
	$(RISCV_GCC) -c $< -o $@
endif

clean:
	rm -f jlc jlc_riscv $(SRC_DIR)/jlc $(SRC_DIR)/jlc_riscv $(COMMON_OBJS) $(SRC_DIR)/CodeGenLLVM.o $(SRC_DIR)/CodeGenRISCV.o $(SRC_DIR)/main.o $(SRC_DIR)/main_riscv.o lib/runtime-riscv.o

distclean: clean
	rm -f \
		$(SRC_DIR)/Absyn.C \
		$(SRC_DIR)/Absyn.H \
		$(SRC_DIR)/Buffer.C \
		$(SRC_DIR)/Buffer.H \
		$(SRC_DIR)/Test.C \
		$(SRC_DIR)/Bison.H \
		$(SRC_DIR)/Parser.C \
		$(SRC_DIR)/Parser.H \
		$(SRC_DIR)/ParserError.H \
		$(SRC_DIR)/Javalette.y \
		$(SRC_DIR)/Lexer.C \
		$(SRC_DIR)/Javalette.l \
		$(SRC_DIR)/Skeleton.C \
		$(SRC_DIR)/Skeleton.H \
		$(SRC_DIR)/Printer.C \
		$(SRC_DIR)/Printer.H \
		$(SRC_DIR)/Javalette.tex
