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

%left '[]'
%noassoc ':='
%left '||'
%left ','
%left '==' '!='
%left '\/'
%left '/\'
%left '!'
%noassoc '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/' '%'
%left NEG

%%

BLOCK :: { AST.BLOCK }
BLOCK : '|[' declare DECLARES INSTRUCTIONS ']|'         { AST.BLOCKD $2 $3 }
      | '|[' INSTRUCTIONS ']|'                          { AST.BLOCK $1 }

DECLARES :: { AST.DECLARES }
DECLARES : DECLARE                                      { AST.DECLARES $1 }
         | DECLARE ';' DECLARES                         { AST.SEQUENCED $1 $3 }

DECLARE :: { AST.DECLARE }
DECLARE : IDLIST ':' TYPELIST                           { AST.DECLARE $1 $3 }

IDLIST :: { [AST.ID] }
IDLIST : varID                                          { [AST.ID $1] }
       | varID ',' IDLIST                               { (AST.ID $1) : $3  }

TYPELIST :: { [AST.TYPE] }
TYPELIST : TYPE                                         { [$1] }
         | TYPE ',' TYPELIST                            { $1 : $3 }

TYPE :: { AST.TYPE }
TYPE : int                                              { AST.INT }
     | bool                                             { AST.BOOL }
     | array '[' n '..' n ']'                           { AST.ARRAY $3 $5 }

INSTRUCTIONS :: { AST.INSTRUCTIONS }
INSTRUCTIONS : INSTRUCTION                              { AST.INST $1 }
             | INSTRUCTION ';' INSTRUCTIONS             { AST.SEQUENCE $1 $3 }

INSTRUCTION :: { AST.INSTRUCTION }
INSTRUCTION : BLOCK                                     { AST.BLOCKINST $1 }
            | varID ':=' INTLIST                        { AST.ASSIGNARRAY $1 $3 }
            | varID ':=' EXPR                           { AST.ASSIGN $1 $3 }
            | read varID                                { AST.READ $2 }
            | print PRINTEXP                            { AST.PRINT $2 }
            | println PRINTEXP                          { AST.PRINTLN $2 }
            | IFINST                                    { AST.IF $1 }
            | FORINST                                   { AST.FOR $1 }
            | DOINST                                    { AST.DO $1 }

EXPR :: { AST.EXPRESSION }
EXPR : INTEXP                                           { AST.INTEXP $1 }
     | BOOLEXP                                          { AST.BOOLEXP $1 }
     | ARRAYEXP                                         { AST.ARRAYEXP $1 }

INTEXP :: { AST.INTEXPRESSION }
INTEXP : INTEXP '+' INTEXP                              { AST.SUM $1 $3 }
       | INTEXP '-' INTEXP                              { AST.MINUS $1 $3 }
       | INTEXP '*' INTEXP                              { AST.MULT $1 $3 }
       | INTEXP '/' INTEXP                              { AST.DIV $1 $3 }
       | INTEXP '%' INTEXP                              { AST.MOD $1 $3 }
       | '(' INTEXP ')'                                 { $2 }
       | '-' INTEXP %prec NEG                           { AST.NEG $2 }
       | ARRAYEXP '[' INTEXP ']'                        { AST.ARRCONSULT $1 $3 }
       | size '(' ARRAYEXP ')'                          { AST.SIZE $3 }
       | atoi '(' ARRAYEXP ')'                          { AST.ATOI $3 }
       | min '(' ARRAYEXP ')'                           { AST.MIN $3 }
       | max '(' ARRAYEXP ')'                           { AST.MAX $3 }
       | varID                                          { AST.INTID $1 }
       | n                                              { AST.INTLITERAL $1 }

BOOLEXP :: { AST.BOOLEXPRESSION }
BOOLEXP : INTEXP '==' INTEXP                            { AST.EQINT $1 $3 }
        | INTEXP '!=' INTEXP                            { AST.NEQINT $1 $3 }
        | INTEXP '<=' INTEXP                            { AST.LEQ $1 $3 }
        | INTEXP '>=' INTEXP                            { AST.GEQ $1 $3 }
        | INTEXP '<' INTEXP                             { AST.LESS $1 $3 }
        | INTEXP '>' INTEXP                             { AST.GREATER $1 $3 }
        | BOOLEXP '==' BOOLEXP                          { AST.EQBOOL $1 $3 }
        | BOOLEXP '!=' BOOLEXP                          { AST.NEQBOOL $1 $3 }
        | BOOLEXP '\/' BOOLEXP                          { AST.OR $1 $3 }
        | BOOLEXP '/\' BOOLEXP                          { AST.AND $1 $3 }
        | '!' BOOLEXP                                   { AST.NOT $1 $3 }
        | '(' BOOLEXP ')'                               { $2 }
        | true                                          { AST.TRUE }
        | false                                         { AST.FALSE }
        | varID                                         { AST.BOOLID $1}

ARRAYEXP :: { AST.ARRAYEXPRESSION }
ARRAYEXP : ARRAYEXP '(' INTEXP ':' INTEXP ')'           { AST.ARRAYMOD $1 $3 $5 }
         | varID                                        { AST.ARRAYID $1 }

INTLIST :: { [AST.INTEXP] }
INTLIST : INTEXP                                        { [$1] }
        | INTEXP ',' INTLIST                            { $1 : $3 }

PRINTEXP :: { AST.PRINTEXP }
PRINTEXP : PRINTEXP '||' PRINTEXP                       { AST.CONCAT $1 $3 }
         | EXPR                                         { AST.EXPR $1 }
         | s                                            { AST.STRINGLIT $1 }

IFINST :: { AST.IF }
IFINST : if GUARDS fi                                   { AST.IF $2 }

DOINST :: { AST.DO }
DOINST : do GUARDS od                                   { AST.DO $2 }

GUARDS :: { AST.GUARDS }
GUARDS : BOOLEXP '->' INSTRUCTIONS                      { AST.GUARDS $1 $3 }
       | GUARDS '[]' GUARDS                             { AST.GUARDSEQ $1 $3 }

FORINST :: { AST.FOR }
FORINST : for varID in INTEXP to INTEXP '->' BLOCK rof  { AST.FOR $2 $4 $6 $8 }