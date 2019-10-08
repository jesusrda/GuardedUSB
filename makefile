all: Main clear

Main: Lexer Tokens
	ghc --make -w Main.hs

Lexer: Lexer.x
	alex Lexer.x

Tokens: Tokens.hs
	ghc --make -w Tokens.hs

clean: clear
	-rm -f Main

clear:
	-rm -f *.hi
	-rm -f *.o
	-rm -f Lexer.hs