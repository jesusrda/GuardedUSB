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
<0>         "int"                               {pushTk TkInt}
<0>         "bool"                              {pushTk TkBool}
<0>         "array"                             {pushTk TkArray}
<0>         "declare"                           {pushTk TkDeclare}
<0>         "read"                              {pushTk TkRead}
<0>         "print"                             {pushTk TkPrint}
<0>         "println"                           {pushTk TkPrintln}
<0>         "if"                                {pushTk TkIf}
<0>         "else"                              {pushTk TkElse}
<0>         "fi"                                {pushTk TkFi}
<0>         "for"                               {pushTk TkFor}
<0>         "to"                                {pushTk TkTo}
<0>         "rof"                               {pushTk TkRof}
<0>         "do"                                {pushTk TkDo}
<0>         "od"                                {pushTk TkOd}
<0>         "atoi"                              {pushTk TkAtoi}
<0>         "size"                              {pushTk TkSize}
<0>         "max"                               {pushTk TkMax}
<0>         "min"                               {pushTk TkMin}

        -- Operators
<0>         \,                                  {skip}
<0>         :                                   {skip}
<0>         \+                                  {skip}
<0>         \-                                  {skip}
<0>         \*                                  {skip}
<0>         \/                                  {skip}
<0>         \%                                  {skip}
<0>         \\\/                                {skip}
<0>         \/\\                                {skip}
<0>         \!                                  {skip}
<0>         \=\=                                {skip}
<0>         \!\=                                {skip}
<0>         \<                                  {skip}
<0>         \<\=                                {skip}
<0>         \>                                  {skip}
<0>         \>\=                                {skip}
<0>         \[\]                                {skip}
<0>         :\=                                 {skip}
<0>         \|\|                                {skip}
<0>         \;                                  {skip}
<0>         \.\.                                {skip}
<0>         \-\-\>                              {skip}

        -- Delimiters
<0>         \(                                  {skip}
<0>         \)                                  {skip}
<0>         \[                                  {skip}
<0>         \]                                  {skip}
<0>         \|\[                                {skip}
<0>         \]\|                                {skip}

        -- Identifiers
<0>         $Alpha [$Alpha $digit \_]*          {skip}

        -- Constants
<0>         $digit+                             {skip}
<0>         "true"                              {skip}
<0>         "false"                             {skip}
<0>         \"                                  {skip}  -- Start string
<stringSt>  \\n                                 {skip}  -- Insert \n to string
<stringSt>  \\\\                                {skip}  -- Insert '\' to string
<stringSt>  \\                                  {skip}  -- Invalid escape
<stringSt>  \"                                  {skip}  -- Leave string
<stringSt>  .                                   {skip}  -- Insert any char to string

        -- Invalid characters
<0>         .                                   {skip}  


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
      | TkEOF
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
    show (TkId s)       = "TkId(" ++ show s ++ ")" 
    show (TkNum n)      = "TkNum(" ++ show n ++ ")" 
    show (TkString s)   = "TkString(" ++ show s ++ ")"
    show TkTrue         = "TkTrue"
    show TkFalse        = "TkFalse"
    show TkEOF          = "TkEOF" 

type TokenPos = (Token, Int, Int)

state_initial :: Int
state_initial = 0

alexEOF :: Alex TokenPos
alexEOF = return (TkEOF, undefined, undefined)

pushTk :: Token -> AlexInput -> Int -> Alex TokenPos
pushTk tok (AlexPn _ l c, _, _, _) len = return (tok, l, c)

main = do
    print "Hola jejeje"
}