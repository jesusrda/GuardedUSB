module Tokens where

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