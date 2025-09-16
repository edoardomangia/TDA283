CXX := g++
CXXFLAGS := -std=c++17 -Wall -Wextra -I.
LDFLAGS := -lfl

SRCDIR := src

SRCS := $(addprefix $(SRCDIR)/, Absyn.C Buffer.C Lexer.C Parser.C Skeleton.C Printer.C \
        TypeChecker.C CodeGen.C main.C)

# Generate object files list in the same way
OBJ := $(SRCS:$(SRCDIR)/%.C=%.o)

# Target rule for the executable
jlc: $(OBJ)
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

# Rule for compiling individual source files
$(OBJ): %.o: $(SRCDIR)/%.C
	$(CXX) $(CXXFLAGS) -c $< -o $@

# Clean rule
clean:
	rm -f jlc $(OBJ)
