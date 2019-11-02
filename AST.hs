module AST where

data BLOCK = BLOCK INSTRUCTIONS 
           | BLOCKD DECLARES INSTRUCTIONS

data DECLARES = DECLARES DECLARE
              | SEQUENCED DECLARE DECLARES

data DECLARE = DECLARE [ID] [TYPE]

newtype ID = ID String

data TYPE = INT 
          | BOOL
          | ARRAY Int Int

data INSTRUCTIONS = INST INSTRUCTION
                  | SEQUENCE INSTRUCTION INSTRUCTIONS

data INSTRUCTION = BLOCKINST BLOCK 
                 | ASSIGNARRAY String [INTEXP]
                 | ASSIGN String EXPRESSION
                 | READ String
                 | PRINT PRINTEXP
                 | PRINTLN PRINTEXP
                 | IFINST IF
                 | DOINST DO
                 | FORINST FOR 

data EXPRESSION = INTEXPR INTEXP
                | BOOLEXPR BOOLEXP
                | ARRAYEXPR ARRAYEXP

data INTEXP = SUM INTEXP INTEXP
            | MINUS INTEXP INTEXP
            | MULT INTEXP INTEXP
            | DIV INTEXP INTEXP
            | MOD INTEXP INTEXP
            | NEG INTEXP
            | ARRELEM ARRAYEXP INTEXP
            | SIZE ARRAYEXP
            | ATOI ARRAYEXP
            | MIN ARRAYEXP
            | MAX ARRAYEXP
            | INTID String
            | INTLIT Int

data BOOLEXP = EQINT INTEXP INTEXP
             | NEQINT INTEXP INTEXP
             | LEQ INTEXP INTEXP
             | GEQ INTEXP INTEXP
             | LESS INTEXP INTEXP
             | GREATER INTEXP INTEXP
             | EQBOOL BOOLEXP BOOLEXP
             | NEQBOOL BOOLEXP BOOLEXP
             | OR BOOLEXP BOOLEXP
             | AND BOOLEXP BOOLEXP
             | NOT BOOLEXP
             | TRUE
             | FALSE
             | BOOLID String

data ARRAYEXP = ARRAYMOD ARRAYEXP INTEXP INTEXP
              | ARRAYID String

data PRINTEXP = CONCAT PRINTEXP PRINTEXP
              | PEXPR EXPRESSION
              | STRINGLIT String

newtype IF = IF GUARDS

newtype DO = DO GUARDS

data FOR = FOR String INTEXP INTEXP BLOCK 

data GUARDS = GUARDS BOOLEXP INSTRUCTIONS
            | GUARDSEQ GUARDS GUARDS
