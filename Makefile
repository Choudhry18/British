main: Lexer.hs Parser.hs Main.hs Eval.hs
	ghc -o bland Main.hs

Lexer.hs: Lexer.x
	alex Lexer.x
	
Parser.hs: Parser.y
	happy Parser.y

clean:
	rm -f bland  
	rm -f Lexer.hs
	rm -f Parser.hs
	rm -f *.o
	rm -f *.hi