
.PHONY : all messyall clean cleantmp
all: messyall cleantmp
	
	
messyall:
	@bison -d asm.y -o asm.cpp
	@flex asm.l
	@g++ -std=gnu++11 -O2 lex.yy.cc asm.cpp main.cpp -o asm
	
cleantmp:
	@rm -f asm.cpp
	@rm -f asm.hpp
	@rm -f lex.yy.cc
	@rm -f location.hh
	@rm -f position.hh
	@rm -f stack.hh
	
clean: cleantmp
	@rm -f asm

