all: GuardedUSB clear

GuardedUSB: Lexer Tokens AST Parser Context
	ghc --make -w GuardedUSB.hs -o guardedusb

Lexer: Lexer.x
	alex Lexer.x

Parser: Parser.y
	happy Parser.y

Tokens: Tokens.hs
	ghc --make -w Tokens.hs

AST: AST.hs
	ghc --make -w AST.hs

Context: ContextChecker.hs
	ghc --make -w ContextChecker.hs

clean: clear
	-rm -f guardedusb

clear:
	-rm -f *.hi
	-rm -f *.o
	-rm -f Lexer.hs
	-rm -f Parser.hs