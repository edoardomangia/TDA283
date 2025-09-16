CXX := g++
CXXFLAGS := -std=c++17 -Wall -Wextra -I.
LDFLAGS := -lfl

SRCS := Absyn.C Buffer.C Lexer.C Parser.C Skeleton.C Printer.C \
        TypeChecker.C CodeGen.C main.C
OBJ  := $(SRCS:.C=.o)

jlc: $(OBJ)
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

%.o: %.C
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -f jlc $(OBJ)
