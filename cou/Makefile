CC	 = cc
GHC	 = ghc

.c.o:
	$(CC) $< -c -o $@

all: cou ccou

cou.hs: ccou.o 

ccou.o: ccou.h ccou.c

cou: cou.hs ccou.o
	$(GHC) -O2 -Wall -rtsopts --make $^

ccou: ccou.c
	$(CC) -O2 -Wall -D_TEST_ $< -o $@
		
clean:
	$(RM) *.o cou ccou *.hi

.PHONY: all clean
