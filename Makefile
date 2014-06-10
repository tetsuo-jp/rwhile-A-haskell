TARGET=RWHILE

# all: Haskell/Interpret

test:
	cd Haskell; bnfc ../$(TARGET).cf;\
	happy -gca Par$(TARGET).y; alex -g Lex$(TARGET).x; ghc --make Interpreter; \
	./Interpreter ../sample/test5.rwhile "cons (cons nil (cons nil nil)) (cons nil (cons nil (cons nil nil)))"
# ./Interpreter ../sample/test6.rwhile "cons (cons nil nil) (cons nil (cons nil nil))"
#       ./Interpreter ../sample/test4.rwhile "cons nil nil"

test_parse:
	cd Haskell; bnfc ../$(TARGET).cf;\
	happy -gca Par$(TARGET).y; alex -g Lex$(TARGET).x; ghc --make TestRWHILE; \
	./TestRWHILE ../sample/test4.rwhile

distclean:
	(cd Haskell; make distclean; rm -f Interpret);

clean:
	cd Haskell; make clean

veryclean: clean
	cd Haskell; rm -f $(TARGET); rm -f LexRWHILE.hs DocRWHILE.tex LexRWHILE.x ParRWHILE.hs ParRWHILE.y Interpreter AbsRWHILE.hs DocRWHILE.txt ErrM.hs PrintRWHILE.hs SkelRWHILE.hs

.PHONY: clean
