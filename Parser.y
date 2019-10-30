{
module Parser where

import Tokens
}

%name parse 
%tokentype { TokenPos }
%error { parseError }

%token
    -- Type Tokens
    int         { (TkInt,_,_) }
    bool        { (TkBool,_,_) }
    array       { (TkArray,_,_) }

    -- Keyword Tokens
    declare     { (TkDeclare,_,_) }
    read        { (TkRead,_,_) }
    print       { (TkPrint,_,_) }
    println     { (TkPrintLn,_,_) }
    if          { (TkIf,_,_) }
    else        { (TkElse,_,_) }
    fi          { (TkFi,_,_) }
    for         { (TkFor,_,_) }
    to          { (TkTo,_,_) }
    rof         { (TkRof,_,_) }
    do          { (TkDo,_,_) }
    od          { (TkOd,_,_) }

    -- Function Tokens
    atoi        { (TkAtoi,_,_) }
    size        { (TkSize,_,_) }
    max         { (TkMax,_,_) }
    min         { (TkMin,_,_) }

    -- Operator Tokens
    ','         { (TkComma,_,_) }
    ':'         { (TkColon,_,_) }
    ';'         { (TkSemicolon,_,_) }
    '..'        { (TkSoForth,_,_) }
    '->'        { (TkArrow,_,_) }
    '+'         { (TkPlus,_,_) }
    '-'         { (TkMinus,_,_) }
    '*'         { (TkMult,_,_) }
    '/'         { (TkDiv,_,_) }
    '%'         { (TkMod,_,_) }
    '\/'        { (TkOr,_,_) }
    '/\'        { (TkAnd,_,_) }
    '!'         { (TkNot,_,_) }
    '=='        { (TkEqual,_,_) }
    '!='        { (TkNEqual,_,_) }
    '<'         { (TkLess,_,_) }
    '<='        { (TkLeq,_,_) }
    '>'         { (TkGreater,_,_) }
    '>='        { (TkGeq,_,_) }
    '[]'        { (TkGuard,_,_) }
    ':='        { (TkAssign,_,_) }
    '||'        { (TkConcat,_,_) }
    '('         { (TkOpenPar,_,_) }
    ')'         { (TkClosePar,_,_) }
    '['         { (TkOpenBracket,_,_) }
    ']'         { (TkCloseBracket,_,_) }
    '|['        { (TkOpenBlock,_,_) }
    ']|'        { (TkCloseBlock,_,_) }

    -- Other Tokens
    varID       { (TkId $$,_,_) }
    n           { (TkNum $$,_,_) }
    s           { (TkString $$,_,_) }
    true        { (TkTrue,_,_) }
    false       { (TkFalse,_,_) }


%%

BLOCK :: { AST.BLOCK }
BLOCK : '|[' declare DECLARES INSTRUCTIONS ']|' { AST.BLOCKD $2 $3 }
      | '|[' INSTRUCTIONS ']|'                  { AST.BLOCK $1 }

DECLARES :: { AST.DECLARES }
DECLARES : DECLARE                              { AST.DECLARES $1 }
         | DECLARE ';' DECLARES                 { AST.SEQUENCE $1 $3 }

DECLARE :: { AST.DECLARE }
DECLARE : IDLIST ':' TYPELIST                   { AST.DECLARE $1 $3 }

IDLIST :: { [AST.ID] }
IDLIST : varID                                  { [AST.ID] }
       | varID ',' IDLIST                       { (AST.ID $1) : $3  }

TYPELIST :: { [AST.TYPE] }
TYPELIST : TYPE                                 { [$1] }
         | TYPE ',' TYPELIST                    { $1 : $3 }

TYPE :: { AST.TYPE }
TYPE : int                                      { AST.INT }
     | bool                                     { AST.BOOL }
     | array '[' n '..' n ']'                   { AST.ARRAY $3 $5 }

INSTRUCTIONS :: { AST.INSTRUCTIONS }
INSTRUCTIONS : INSTRUCTION                      { AST.INST $1 }
             | INSTRUCTION ';' INSTRUCTIONS     { AST.SEQUENCE $1 $3 }

INSTRUCTION :: 