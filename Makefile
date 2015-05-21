TARGET=rw

all:
	cd src;                  \
	bnfc -m ../RWHILE.cf;    \
	happy -gca ParRWHILE.y;  \
	alex -g LexRWHILE.x;     \
	ghc -o $(TARGET) --make Interpreter

test:
	src/rw sample/test5.rwhile "cons (cons nil (cons nil nil)) (cons nil (cons nil (cons nil nil)))"

distclean:
	(cd src; make distclean; rm -f $(TARGET));

clean:
	cd src; make clean

.PHONY: clean
