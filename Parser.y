{
module Parser where

import Tokens
}

%name parse 
%tokentype { TokenPos }
%error { parseError }

%token
	-- Type Tokens
	int			{ (TkInt,_,_) }
	bool		{ (TkBool,_,_) }
	array		{ (TkArray,_,_) }

	-- Keyword Tokens
	declare		{ (TkDeclare,_,_) }
	read		{ (TkRead,_,_) }
	print		{ (TkPrint,_,_) }
	println 	{ (TkPrintLn,_,_) }
	if			{ (TkIf,_,_) }
	else		{ (TkElse,_,_) }
	fi			{ (TkFi,_,_) }
	for			{ (TkFor,_,_) }
	to 			{ (TkTo,_,_) }
	rof			{ (TkRof,_,_) }
	do 			{ (TkDo,_,_) }
	od 			{ (TkOd,_,_) }

	-- Function Tokens
	atoi 		{ (TkAtoi,_,_) }
	size 		{ (TkSize,_,_) }
	max 		{ (TkMax,_,_) }
	min 		{ (TkMin,_,_) }

	-- Operator Tokens
	',' 		{ (TkComma,_,_) }
	':'			{ (TkColon,_,_) }
	';' 		{ (TkSemicolon,_,_) }
	'..'		{ (TkSoForth,_,_) }
	'->' 		{ (TkArrow,_,_) }
	'+'			{ (TkPlus,_,_) }
	'-'			{ (TkMinus,_,_) }
	'*'			{ (TkMult,_,_) }
	'/'			{ (TkDiv,_,_) }
	'%'			{ (TkMod,_,_) }
	'\/'		{ (TkOr,_,_) }
	'/\'		{ (TkAnd,_,_) }
	'!' 		{ (TkNot,_,_) }
	'=='		{ (TkEqual,_,_) }
	'!='		{ (TkNEqual,_,_) }
	'<'			{ (TkLess,_,_) }
	'<='		{ (TkLeq,_,_) }
	'>'			{ (TkGreater,_,_) }
	'>='		{ (TkGeq,_,_) }
	'[]' 		{ (TkGuard,_,_) }
	':='		{ (TkAssign,_,_) }
	'||'		{ (TkConcat,_,_) }
	'('			{ (TkOpenPar,_,_) }
	')' 		{ (TkClosePar,_,_) }
	'['			{ (TkOpenBracket,_,_) }
	']'			{ (TkCloseBracket,_,_) }
	'|['		{ (TkOpenBlock,_,_) }
	']|'		{ (TkCloseBlock,_,_) }

	-- Other Tokens
	varId 		{ (TkId _,_,_) }
	n 			{ (TkNum _,_,_) }
	s 			{ (TkString _,_,_) }
	true 		{ (TkTrue,_,_) }
	false 		{ (TkFalse,_,_) }

%%
