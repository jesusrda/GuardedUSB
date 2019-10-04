{
module Lexer () where
}

%wrapper "monad"

$digit = 0-9
$Alpha = [a-zA-Z]
$alpha = [a-z]
$ALPHA = [A-Z]

tokens :-
<0>			$white+								;
<0>			"//".*								;

	-- Reserved words
<0>			"int"								{}
<0>			"bool" 								{}
<0>			"array"								{}
<0>			"declare"							{}
<0>			"read"								{}
<0>			"print"								{}
<0>			"println"							{}
<0>			"if"								{}
<0>			"fi"								{}
<0>			"for"								{}
<0>			"rof"								{}
<0>			"do"								{}
<0>			"od"								{}
<0>			"atoi"								{}
<0>			"size"								{}
<0>			"max"								{}
<0>			"min"								{}

	-- Operators
<0>			\,									{}
<0>			:									{}
<0>			\+									{}
<0>			\-									{}
<0>			\*									{}
<0>			\/									{}
<0>			\%									{}
<0>			\\\/								{}
<0>			\/\\ 								{}
<0>			\!									{}
<0>			\=\=								{}
<0>			\!\=								{}
<0>			\<									{}
<0>			\<\= 								{}
<0>			\> 									{}
<0>			\>\= 								{}
<0>			\[\]								{}
<0>			:\=									{}
<0>			\|\|								{}
<0>			\;									{}

	-- Delimiters
<0>			\(									{}
<0>			\)									{}
<0>			\[									{}
<0>			\]									{}
<0>			\|\[								{}
<0>			\]\|								{}

	-- Identifiers
<0>			$Alpha [$Alpha $digit \_]*			{}

	-- Constants
<0>			$digit+								{}
<0>			\"									{} 	-- Start string
<stringSt>	\\n									{}	-- Insert \n to string
<stringSt>	\\\\ 								{}	-- Insert '\' to string
<stringSt>	\\ 									{}	-- Invalid escape
<stringSt>	\"									{}	-- Leave string
<stringSt>	. 									{}	-- Insert any char to string
