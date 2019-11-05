module AST where

data BLOCK = BLOCK INSTRUCTIONS 
           | BLOCKD DECLARES INSTRUCTIONS

data DECLARES = DECLARES DECLARE
              | SEQUENCED DECLARES DECLARE

data DECLARE = UNIQUETYPE [ID] TYPE
             | MULTITYPE [ID] [TYPE]

newtype ID = ID String

data TYPE = INT 
          | BOOL
          | ARRAY Int Int

data INSTRUCTIONS = INST INSTRUCTION
                  | SEQUENCE INSTRUCTIONS INSTRUCTION

data INSTRUCTION = BLOCKINST BLOCK 
                 | ASSIGNARRAY String [EXPR]
                 | ASSIGN String EXPR
                 | READ String
                 | PRINT PRINTEXP
                 | PRINTLN PRINTEXP
                 | IFINST IF
                 | DOINST DO
                 | FORINST FOR 

data EXPR = SUM EXPR EXPR
          | MINUS EXPR EXPR
          | MULT EXPR EXPR
          | DIV EXPR EXPR
          | MOD EXPR EXPR
          | ARRELEM EXPR EXPR
          | EQ EXPR EXPR
          | NEQ EXPR EXPR
          | LEQ EXPR EXPR
          | GEQ EXPR EXPR
          | LESS EXPR EXPR
          | GREATER EXPR EXPR
          | OR EXPR EXPR
          | AND EXPR EXPR
          | NOT EXPR
          | NEG EXPR
          | ARRAYMOD EXPR EXPR EXPR
          | SIZE EXPR
          | ATOI EXPR
          | MIN EXPR
          | MAX EXPR
          | IDT String
          | TRUE
          | FALSE
          | NUM Int

data PRINTEXP = CONCAT PRINTEXP PRINTEXP
              | PEXPR EXPR
              | STRINGLIT String

newtype IF = IF GUARDS

newtype DO = DO GUARDS

data FOR = FOR String EXPR EXPR BLOCK 

data GUARDS = GUARDS EXPR INSTRUCTIONS
            | GUARDSEQ GUARDS GUARDS
