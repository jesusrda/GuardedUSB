{
module Parser where

import Tokens
import qualified AST
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
    println     { (TkPrintln,_,_) }
    if          { (TkIf,_,_) }
    fi          { (TkFi,_,_) }
    for         { (TkFor,_,_) }
    in          { (TkIn,_,_) }
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
    'v'         { (TkOr,_,_) }
    '^'         { (TkAnd,_,_) }
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
%left ';'
%nonassoc ':='
%left '||'
%left ':'
%left ','
%left '==' '!='
%left 'v'
%left '^'
%left '!'
%nonassoc '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/' '%'
%left NEG
%left '(' '['

%%

BLOCK :: { AST.BLOCK }
BLOCK : '|[' declare DECLARES INSTRUCTIONS ']|'         { AST.BLOCKD $3 $4 }
      | '|[' INSTRUCTIONS ']|'                          { AST.BLOCK $2 }

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
            | varID ':=' EXPLIST                        { AST.ASSIGNARRAY $1 $3 }
            | varID ':=' EXPR                           { AST.ASSIGN $1 $3 }
            | read varID                                { AST.READ $2 }
            | print PRINTEXP                            { AST.PRINT $2 }
            | println PRINTEXP                          { AST.PRINTLN $2 }
            | IF                                        { AST.IFINST $1 }
            | FOR                                       { AST.FORINST $1 }
            | DO                                        { AST.DOINST $1 }

EXPR :: { AST.EXPR }
EXPR : EXPR '+' EXPR                                    { AST.SUM $1 $3 }
     | EXPR '-' EXPR                                    { AST.MINUS $1 $3 }
     | EXPR '*' EXPR                                    { AST.MULT $1 $3 }
     | EXPR '/' EXPR                                    { AST.DIV $1 $3 }
     | EXPR '%' EXPR                                    { AST.MOD $1 $3 }
     | EXPR '[' EXPR ']'                                { AST.ARRELEM $1 $3 }
     | EXPR '==' EXPR                                   { AST.EQ $1 $3 }
     | EXPR '!=' EXPR                                   { AST.NEQ $1 $3 }
     | EXPR '<=' EXPR                                   { AST.LEQ $1 $3 }
     | EXPR '>=' EXPR                                   { AST.GEQ $1 $3 }
     | EXPR '<' EXPR                                    { AST.LESS $1 $3 }
     | EXPR '>' EXPR                                    { AST.GREATER $1 $3 }
     | EXPR 'v' EXPR                                    { AST.OR $1 $3 }
     | EXPR '^' EXPR                                    { AST.AND $1 $3 }
     | '(' EXPR ')'                                     { $2 }
     | '!' EXPR                                         { AST.NOT $2 }
     | '-' EXPR %prec NEG                               { AST.NEG $2 }
     | EXPR '(' EXPR ':' EXPR ')'                       { AST.ARRAYMOD $1 $3 $5 }
     | size '(' EXPR ')'                                { AST.SIZE $3 }
     | atoi '(' EXPR ')'                                { AST.ATOI $3 }
     | min '(' EXPR ')'                                 { AST.MIN $3 }
     | max '(' EXPR ')'                                 { AST.MAX $3 }
     | varID                                            { AST.IDT $1 }
     | false                                            { AST.FALSE }
     | true                                             { AST.TRUE }
     | n                                                { AST.NUM $1 }

EXPLIST :: { [AST.EXPR] }
EXPLIST : {-λ-}                                         { [] }
        | EXPR ',' EXPLIST                              { $1 : $3 }

PRINTEXP :: { AST.PRINTEXP }
PRINTEXP : PRINTEXP '||' PRINTEXP                       { AST.CONCAT $1 $3 }
         | EXPR                                         { AST.PEXPR $1 }
         | s                                            { AST.STRINGLIT $1 }

IF :: { AST.IF }
IF : if GUARDS fi                                       { AST.IF $2 }

DO :: { AST.DO }
DO : do GUARDS od                                       { AST.DO $2 }

FOR :: { AST.FOR }
FOR : for varID in EXPR to EXPR '->' BLOCK rof      { AST.FOR $2 $4 $6 $8 }

GUARDS :: { AST.GUARDS }
GUARDS : EXPR '->' INSTRUCTIONS                      { AST.GUARDS $1 $3 }
       | GUARDS '[]' GUARDS                             { AST.GUARDSEQ $1 $3 }

{

parseError :: [TokenPos] -> a
parseError tks = error $ "Parser Error at " ++ msg ++ ": Alex is not happy :c"
    where 
        msg = case tks of
                [] -> "the end of file"
                ((_,l,_):_) -> "line " ++ show l
}