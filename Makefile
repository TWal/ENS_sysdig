.PHONY : all

all:
	cabal build
	./run.sh > Proco.net
	NetCompil/bin/netCompil Proco.net -o Proco.cpp
	g++ Proco.cpp -o proco
	
