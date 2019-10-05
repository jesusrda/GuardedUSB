{
module Lexer (scanner, printTokenPos) where
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
<0>         \,                                  {pushTk TkComma}
<0>         :                                   {pushTk TkColon}
<0>         \+                                  {pushTk TkPlus}
<0>         \-                                  {pushTk TkMinus}
<0>         \*                                  {pushTk TkMult}
<0>         \/                                  {pushTk TkDiv}
<0>         \%                                  {pushTk TkMod}
<0>         \\\/                                {pushTk TkOr}
<0>         \/\\                                {pushTk TkAnd}
<0>         \!                                  {pushTk TkNot}
<0>         \=\=                                {pushTk TkEqual}
<0>         \!\=                                {pushTk TkNEqual}
<0>         \<                                  {pushTk TkLess}
<0>         \<\=                                {pushTk TkLeq}
<0>         \>                                  {pushTk TkGreater}
<0>         \>\=                                {pushTk TkGeq}
<0>         \[\]                                {pushTk TkGuard}
<0>         :\=                                 {pushTk TkAssign}
<0>         \|\|                                {pushTk TkConcat}
<0>         \;                                  {pushTk TkSemicolon}
<0>         \.\.                                {pushTk TkSoForth}
<0>         \-\-\>                              {pushTk TkArrow}

        -- Delimiters
<0>         \(                                  {pushTk TkOpenPar}
<0>         \)                                  {pushTk TkClosePar}
<0>         \[                                  {pushTk TkOpenBracket}
<0>         \]                                  {pushTk TkCloseBracket}
<0>         \|\[                                {pushTk TkOpenBlock}
<0>         \]\|                                {pushTk TkCloseBlock}

        -- Identifiers
<0>         $Alpha [$Alpha $digit \_]*          {pushId}

        -- Constants
<0>         $digit+                             {pushNum}
<0>         "true"                              {pushTk TkTrue}
<0>         "false"                             {pushTk TkFalse}
<0>         \"                                  {enterString `andBegin` stringSt}        -- Start string
<stringSt>  \\n                                 {addChar '\n'}                          -- Insert \n to string
<stringSt>  \\\\                                {addChar '\\'}                          -- Insert '\' to string
<stringSt>  \\\"                                {addChar '\"'}
<stringSt>  \\                                  {skip}                                  -- Invalid escape
<stringSt>  \"                                  {leaveString `andBegin` stateInitial}   -- Leave string
<stringSt>  .                                   {addCurrentChar}                        -- Insert any char to string

        -- Invalid characters
<0>         .                                   {pushInvalid}  


{
type LexerError = String

-- Initial State
stateInitial :: Int
stateInitial = 0

-- Definition needed by Alex
alexEOF :: Alex TokenPos
alexEOF = return (TkEOF, undefined, undefined)

-- User state
data AlexUserState = AlexUserState
                    {
                        lexerStringState :: Bool,
                        lexerStringValue :: String,
                        lexerErrors :: [LexerError]
                    }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
                    {
                        lexerStringState = False ,
                        lexerStringValue = "" ,
                        lexerErrors = []
                    }

-- Push Tokens
pushTk :: Token -> AlexInput -> Int -> Alex TokenPos
pushTk tok (AlexPn _ l c, _, _, _) _ = return (tok, l, c)

pushNum :: AlexInput -> Int -> Alex TokenPos
pushNum (AlexPn _ l c, _, _, inp) len = return $ (TkNum $ read $ take len inp, l, c)

pushId :: AlexInput -> Int -> Alex TokenPos
pushId (AlexPn _ l c, _, _, inp) len = return $ (TkId $ take len inp, l, c)

pushInvalid :: AlexInput -> Int -> Alex TokenPos
pushInvalid (AlexPn _ l c, _, _, inp) len = 
                                            do
                                                ust@AlexUserState{lexerErrors = lexErr} <- alexGetUserState
                                                alexSetUserState $ ust{lexerErrors = errorMsg : lexErr}
                                                alexMonadScan
                                            where errorMsg = "error: Unexpected char '" ++ (take len inp) ++ "' at line "
                                                                ++ show l ++ " column " ++ show c

printTokenPos :: TokenPos -> IO ()
printTokenPos (tk, l, c) = putStrLn $ show tk ++ " " ++ show l ++ " " ++ show c 

-- String mode
enterString :: AlexAction TokenPos
enterString _ _ = undefined

addChar :: Char -> AlexAction TokenPos
addChar _ _ _ = undefined

addCurrentChar :: AlexAction TokenPos
addCurrentChar _ _ = undefined

leaveString :: AlexAction TokenPos
leaveString _ _ = undefined


-- Scanner
scanner :: String -> Either String [TokenPos]
scanner str = 
    let loop = do
        tkPos@(tok,l,c) <- alexMonadScan
        if tok == TkEOF
        then do
                AlexUserState{lexerErrors = lexErr} <- alexGetUserState
                if null lexErr
                then return []
                else alexError $ unlines $ reverse lexErr
        else do toks <- loop
                return (tkPos : toks)
    in runAlex str loop 

}