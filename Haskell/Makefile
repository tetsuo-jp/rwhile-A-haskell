all: 
	happy -gca -iParRWHILE_error.txt ParRWHILE.y
	alex -g LexRWHILE.x
	# ghc --make TestRWHILE.hs -o TestRWHILE
	ghc --make Interpreter.hs -o Interpreter

doc: DocRWHILE.pdf

DocRWHILE.pdf: DocRWHILE.tex
	pdflatex DocRWHILE.tex

clean: 
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocRWHILE.ps

distclean: clean
	-rm -f DocRWHILE.* LexRWHILE.* ParRWHILE.* LayoutRWHILE.* SkelRWHILE.* PrintRWHILE.* TestRWHILE.* AbsRWHILE.* TestRWHILE ErrM.* SharedString.* ComposOpRWHILE.* RWHILE.dtd XMLRWHILE.* Makefile*
