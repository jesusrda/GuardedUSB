all: Main clear

Main: Lexer Tokens AST Parser
	ghc --make -w Main.hs

Lexer: Lexer.x
	alex Lexer.x

Parser: Parser.y
	happy Parser.y

Tokens: Tokens.hs
	ghc --make -w Tokens.hs

AST: AST.hs
	ghc --make -w AST.hs

clean: clear
	-rm -f Main

clear:
	-rm -f *.hi
	-rm -f *.o
	-rm -f Lexer.hs
	-rm -f Parser.hs