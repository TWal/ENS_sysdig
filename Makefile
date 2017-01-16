.PHONY : all clean mrproper spec clock test proco

all: NetCompil/bin/netCompil proco
	

proco:
	cabal build
	./run.sh > proco.net
	NetCompil/bin/netCompil proco.net -o proco.cpp
	g++ -O3 --std=c++14 proco.cpp -o proco

NetCompil/bin/netCompil:
	make -C NetCompil

clean:
	rm -f proco
	rm -f proco.net
	rm -f proco.cpp

mrproper: clean
	make mrproper -C NetCompil

spec: Specification/spec.tex
	make -C Specification

clock:
	make -C clock
	
testclock : clock proco
	./proco --rom clock/a.out --ram 100000 > log
	
test: proco
	asm/asm test.s -o prog
	./proco --rom prog --ram 100000 > log 
