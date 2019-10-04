{
module Lexer () where
}

%wrapper "monad"

$digit = 0-9
$Alpha = [a-zA-Z]
$alpha = [a-z]
$ALPHA = [A-Z]

tokens :-
<0>         $white+                             ;
<0>         "//".*                              ;

        -- Reserved words
<0>         "int"                               {}
<0>         "bool"                              {}
<0>         "array"                             {}
<0>         "declare"                           {}
<0>         "read"                              {}
<0>         "print"                             {}
<0>         "println"                           {}
<0>         "if"                                {}
<0>         "else"                              {}
<0>         "fi"                                {}
<0>         "for"                               {}
<0>         "to"                                {}
<0>         "rof"                               {}
<0>         "do"                                {}
<0>         "od"                                {}
<0>         "atoi"                              {}
<0>         "size"                              {}
<0>         "max"                               {}
<0>         "min"                               {}

        -- Operators
<0>         \,                                  {}
<0>         :                                   {}
<0>         \+                                  {}
<0>         \-                                  {}
<0>         \*                                  {}
<0>         \/                                  {}
<0>         \%                                  {}
<0>         \\\/                                {}
<0>         \/\\                                {}
<0>         \!                                  {}
<0>         \=\=                                {}
<0>         \!\=                                {}
<0>         \<                                  {}
<0>         \<\=                                {}
<0>         \>                                  {}
<0>         \>\=                                {}
<0>         \[\]                                {}
<0>         :\=                                 {}
<0>         \|\|                                {}
<0>         \;                                  {}
<0>         \.\.                                {}
<0>         \-\-\>                              {}

        -- Delimiters
<0>         \(                                  {}
<0>         \)                                  {}
<0>         \[                                  {}
<0>         \]                                  {}
<0>         \|\[                                {}
<0>         \]\|                                {}

        -- Identifiers
<0>         $Alpha [$Alpha $digit \_]*          {}

        -- Constants
<0>         $digit+                             {}
<0>         "true"                              {}
<0>         "false"                             {}
<0>         \"                                  {}  -- Start string
<stringSt>  \\n                                 {}  -- Insert \n to string
<stringSt>  \\\\                                {}  -- Insert '\' to string
<stringSt>  \\                                  {}  -- Invalid escape
<stringSt>  \"                                  {}  -- Leave string
<stringSt>  .                                   {}  -- Insert any char to string

{
-- Token type
data Token = 
        TkInt
      | TkBool
      | TkArray
      | TkDeclare
      | TkRead
      | TkPrint
      | TkPrintln
      | TkIf
      | TkElse
      | TkFi
      | TkFor
      | TkTo
      | TkRof
      | TkDo
      | TkOd
      | TkAtoi
      | TkSize
      | TkMax
      | TkMin
      | TkComma
      | TkColon
      | TkSemicolon
      | TkSoForth
      | TkArrow
      | TkPlus
      | TkMinus
      | TkMult
      | TkDiv
      | TkMod
      | TkOr
      | TkAnd
      | TkNot
      | TkEqual
      | TkNEqual
      | TkLess
      | TkLeq
      | TkGreater
      | TkGeq
      | TkGuard
      | TkAssign
      | TkConcat
      | TkOpenPar
      | TkClosePar
      | TkOpenBracket
      | TkCloseBracket
      | TkOpenBlock
      | TkCloseBlock
      | TkId String
      | TkNum Int
      | TkString String
      | TkTrue
      | TkFalse
      deriving(Eq)

instance Show Token where
    show TkInt          = "TkInt"
    show TkBool         = "TkBool"
    show TkArray        = "TkArray"
    show TkDeclare      = "TkDeclare"
    show TkRead         = "TkRead"
    show TkPrint        = "TkPrint"
    show TkPrintln      = "TkPrintln"
    show TkIf           = "TkIf"
    show TkElse         = "TkElse"
    show TkFi           = "TkFi"
    show TkFor          = "TkFor"
    show TkTo           = "TkTo"
    show TkRof          = "TkRof"
    show TkDo           = "TkDo"
    show TkOd           = "TkOd"
    show TkAtoi         = "TkAtoi"
    show TkSize         = "TkSize"
    show TkMax          = "TkMax"
    show TkMin          = "TkMin"
    show TkComma        = "TkComma"
    show TkColon        = "TkTwoPoints"
    show TkSemicolon    = "TkSemicolon"
    show TkSoForth      = "TkSoForth"
    show TkArrow        = "TkArrow"
    show TkPlus         = "TkPlus"
    show TkMinus        = "TkMinus"
    show TkMult         = "TkMult"
    show TkDiv          = "TkDiv"
    show TkMod          = "TkMod"
    show TkOr           = "TkOr"
    show TkAnd          = "TkAnd"
    show TkNot          = "TkNot" 
    show TkEqual        = "TkEqual"
    show TkNEqual       = "TkNEqual"
    show TkLess         = "TkLess"
    show TkLeq          = "TkLeq"
    show TkGreater      = "TkGreater"
    show TkGeq          = "TkGeq"
    show TkGuard        = "TkGuard"
    show TkAssign       = "TkAsig"
    show TkConcat       = "TkConcat"
    show TkOpenPar      = "TkOpenPar"
    show TkClosePar     = "TkClosePar"
    show TkOpenBracket  = "TkOBracket"
    show TkCloseBracket = "TkCBracket"
    show TkOpenBlock    = "TkOBlock"
    show TkCloseBlock   = "TkCBlock"
    show TkId s         = "TkId(" ++ show s ++ ")" 
    show TkNum n        = "TkNum(" ++ show n ++ ")" 
    show TkString s     = "TkString(" ++ show s ++ ")"
    show TkTrue         = "TkTrue"
    show TkFalse        = "TkFalse"
       
}