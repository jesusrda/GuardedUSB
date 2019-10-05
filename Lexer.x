{
module Lexer (scanner) where
import Tokens
}

%wrapper "monadUserState"

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
-- Initial State
stateInitial :: Int
stateInitial = 0

-- Definition needed by Alex
alexEOF :: Alex TokenPos
alexEOF = return (TkEOF, undefined, undefined)

-- User state
data AlexUserState = AlexUserState
                    {
                        lexerStringState :: Bool  ,
                        lexerStringValue :: String
                    }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
                    {
                        lexerStringState = False ,
                        lexerStringValue = ""
                    }

pushTk :: Token -> AlexInput -> Int -> Alex TokenPos
pushTk tok (AlexPn _ l c, _, _, _) len = return (tok, l, c)

scanner :: String -> Either String [TokenPos]
scanner str = 
    let loop = do
        tkPos@(tok,l,c) <- alexMonadScan
        if tok == TkEOF
        then return []
        else do toks <- loop
                return (tkPos : toks)
    in runAlex str loop 


}