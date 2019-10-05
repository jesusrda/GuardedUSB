Main: Lexer Tokens
	ghc --make -w Main.hs

Lexer: Lexer.x
	alex Lexer.x

Tokens: Tokens.hs
	ghc --make -w Tokens.hs

clean:
	-rm -f *.hi
	-rm -f *.o
	-rm -f Lexer.hs
	-rm -f Main