CXX = g++
CXXFLAGS = -Wall -Wpedantic -Wextra \
	 -Wno-unused-variable -Wno-unused-parameter -Wno-unused-function \
	 -march=native -std=c++14
TEST = -Og -g
PROD = -O3

DEPS = ../quaint.hpp unittest.cpp test.cpp

test: test.out
.PHONY: test

test.out: test.cpp $(DEPS)
	$(CXX) $(CXXFLAGS) $(TEST) $< -o $@ && ./$@

clean:
	rm *.out
