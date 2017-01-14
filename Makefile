.PHONY : all clean mrproper spec

all: NetCompil/bin/netCompil
	cabal build
	./run.sh > proco.net
	NetCompil/bin/netCompil proco.net -o proco.cpp
	g++ proco.cpp -o proco

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
