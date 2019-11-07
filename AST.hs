module AST where

import Control.Monad

-- Block has declares and instructions or just instructions
data BLOCK = BLOCK INSTRUCTIONS 
           | BLOCKD DECLARES INSTRUCTIONS

-- Multiple declare statements
data DECLARES = DECLARES DECLARE
              | SEQUENCED DECLARES DECLARE

-- Single declare statement
data DECLARE = UNIQUETYPE [ID] TYPE
             | MULTITYPE [ID] [TYPE]

-- ID
newtype ID = ID String

-- Type declaration
data TYPE = INT 
          | BOOL
          | ARRAY Int Int

-- Multiple instructions
data INSTRUCTIONS = INST INSTRUCTION
                  | SEQUENCE INSTRUCTIONS INSTRUCTION

-- Single instruction
data INSTRUCTION = BLOCKINST BLOCK 
                 | ASSIGNARRAY String [EXPR]
                 | ASSIGN String EXPR
                 | READ String
                 | PRINT PRINTEXP
                 | PRINTLN PRINTEXP
                 | IFINST IF
                 | DOINST DO
                 | FORINST FOR 

-- Arithmetic, boolean and array expressions
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

-- Printable expression (can include string literals and concatenation)
data PRINTEXP = CONCAT PRINTEXP PRINTEXP
              | PEXPR EXPR
              | STRINGLIT String

-- If statement
newtype IF = IF GUARDS

-- Do statement
newtype DO = DO GUARDS

-- For statement
data FOR = FOR String EXPR EXPR BLOCK 

-- Guards with condition and instructions for if and do statements
data GUARDS = GUARDS EXPR INSTRUCTIONS
            | GUARDSEQ GUARDS GUARDS


-- Function used to print an identation space and then a string
putStrIdent :: Int -> String -> IO ()
putStrIdent n str = do replicateM_ n (putStr "  ")
                       putStrLn str

 -- Functions used to print every node of the tree
printAST :: BLOCK -> IO ()
printAST block = printBLOCK 0 block

printBLOCK :: Int -> BLOCK -> IO ()
printBLOCK d (BLOCK inst) = do putStrIdent d "Block"
                               printINSTS (d+1) inst
printBLOCK d (BLOCKD decs inst) = do putStrIdent d "Block"
                                     putStrIdent (d+1) "Declare"
                                     printDECS (d+2) decs 
                                     printINSTS (d+1) inst

printDECS :: Int -> DECLARES -> IO ()
printDECS d (DECLARES dec) = printDEC d dec 
printDECS d (SEQUENCED decs dec) = do putStrIdent d "Sequencing dec"
                                      printDECS (d+1) decs
                                      printDEC (d+1) dec

printDEC :: Int -> DECLARE -> IO () 
printDEC d (UNIQUETYPE ids _) = mapM_ (printID d) ids
printDEC d (MULTITYPE ids _) = mapM_ (printID d) ids

printID :: Int -> ID -> IO ()
printID d (ID id) = putStrIdent d ("ID: " ++ id) 

printINSTS :: Int -> INSTRUCTIONS -> IO ()
printINSTS d (INST inst) = printINST d inst
printINSTS d (SEQUENCE insts inst) = do putStrIdent d "Sequencing inst" 
                                        printINSTS (d+1) insts
                                        printINST (d+1) inst 

printINST :: Int -> INSTRUCTION -> IO ()
printINST d (BLOCKINST block) = printBLOCK d block
printINST d (ASSIGNARRAY id exps) = do putStrIdent d "AssignArray"
                                       printID (d+1) (ID id)
                                       mapM_ (printEXPR (d+1)) exps

printINST d (ASSIGN id exp) = do putStrIdent d "Assign"
                                 printID (d+1) (ID id)
                                 printEXPR (d+1) exp

printINST d (READ id) = do putStrIdent d "Read"
                           putStrIdent (d+1) ("ID: " ++ id) 

printINST d (PRINT pexp) = do putStrIdent d "Print"
                              printPEXP (d+1) pexp 

printINST d (PRINTLN pexp) = do putStrIdent d "PrintLn"
                                printPEXP (d+1) pexp 
 
printINST d (IFINST ifinst) = printIF d ifinst
printINST d (DOINST doinst) = printDO d doinst
printINST d (FORINST forinst) = printFOR d forinst

printEXPR :: Int -> EXPR -> IO ()
printEXPR d (SUM exp1 exp2) = do putStrIdent d "Plus"
                                 printEXPR (d+1) exp1
                                 printEXPR (d+1) exp2

printEXPR d (MINUS exp1 exp2) = do putStrIdent d "Minus"
                                   printEXPR (d+1) exp1
                                   printEXPR (d+1) exp2

printEXPR d (MULT exp1 exp2) = do putStrIdent d "Mult"
                                  printEXPR (d+1) exp1
                                  printEXPR (d+1) exp2

printEXPR d (DIV exp1 exp2) = do putStrIdent d "Div"
                                 printEXPR (d+1) exp1
                                 printEXPR (d+1) exp2

printEXPR d (MOD exp1 exp2) = do putStrIdent d "Mod"
                                 printEXPR (d+1) exp1
                                 printEXPR (d+1) exp2

printEXPR d (ARRELEM exp1 exp2) = do putStrIdent d "ArrayElement"
                                     printEXPR (d+1) exp1
                                     printEXPR (d+1) exp2

printEXPR d (AST.EQ exp1 exp2) = do putStrIdent d "Equal"
                                    printEXPR (d+1) exp1
                                    printEXPR (d+1) exp2

printEXPR d (NEQ exp1 exp2) = do putStrIdent d "NotEqual"
                                 printEXPR (d+1) exp1
                                 printEXPR (d+1) exp2

printEXPR d (LEQ exp1 exp2) = do putStrIdent d "LessEqual"
                                 printEXPR (d+1) exp1
                                 printEXPR (d+1) exp2

printEXPR d (GEQ exp1 exp2) = do putStrIdent d "GreaterEqual"
                                 printEXPR (d+1) exp1
                                 printEXPR (d+1) exp2

printEXPR d (LESS exp1 exp2) = do putStrIdent d "Less"
                                  printEXPR (d+1) exp1
                                  printEXPR (d+1) exp2

printEXPR d (GREATER exp1 exp2) = do putStrIdent d "Greater"
                                     printEXPR (d+1) exp1
                                     printEXPR (d+1) exp2

printEXPR d (OR exp1 exp2) = do putStrIdent d "Or"
                                printEXPR (d+1) exp1
                                printEXPR (d+1) exp2

printEXPR d (AND exp1 exp2) = do putStrIdent d "And"
                                 printEXPR (d+1) exp1
                                 printEXPR (d+1) exp2

printEXPR d (NOT exp) = do putStrIdent d "Not"
                           printEXPR (d+1) exp

printEXPR d (NEG exp) = do putStrIdent d "Negate"
                           printEXPR (d+1) exp

printEXPR d (ARRAYMOD exp1 exp2 exp3) = do putStrIdent d "ModifyArray"
                                           printEXPR (d+1) exp1
                                           printEXPR (d+1) exp2
                                           printEXPR (d+1) exp3

printEXPR d (SIZE exp) = do putStrIdent d "Size"
                            printEXPR (d+1) exp

printEXPR d (ATOI exp) = do putStrIdent d "Atoi"
                            printEXPR (d+1) exp

printEXPR d (MIN exp) = do putStrIdent d "Min"
                           printEXPR (d+1) exp

printEXPR d (MAX exp) = do putStrIdent d "Max"
                           printEXPR (d+1) exp

printEXPR d (SIZE exp) = do putStrIdent d "Size"
                            printEXPR (d+1) exp

printEXPR d (IDT id) = putStrIdent d ("ID: " ++ id) 
printEXPR d TRUE = putStrIdent d "True"
printEXPR d FALSE = putStrIdent d "False"
printEXPR d (NUM n) = putStrIdent d ("Literal: " ++ show n)


printPEXP :: Int -> PRINTEXP -> IO ()
printPEXP d (CONCAT exp1 exp2) = do putStrIdent d "Concat"
                                    printPEXP (d+1) exp1
                                    printPEXP (d+1) exp2
printPEXP d (PEXPR exp) = printEXPR d exp 
printPEXP d (STRINGLIT s) = putStrIdent d ("\"" ++ s ++ "\"")


printIF :: Int -> IF -> IO ()
printIF d (IF gs) = do putStrIdent d "If"
                       printGUARDS (d+1) gs

printDO :: Int -> DO -> IO ()
printDO d (DO gs) = do putStrIdent d "Do"
                       printGUARDS (d+1) gs

printGUARDS :: Int -> GUARDS -> IO ()
printGUARDS d (GUARDS exp insts) = do putStrIdent d "Guard"
                                      printEXPR (d+1) exp
                                      printINSTS (d+1) insts
printGUARDS d (GUARDSEQ g1 g2) = do printGUARDS d g1
                                    printGUARDS d g2

printFOR :: Int -> FOR -> IO ()
printFOR d (FOR id exp1 exp2 block) = do putStrIdent d "For"
                                         putStrIdent (d+1) ("ID: " ++ id)
                                         putStrIdent (d+2) "In"
                                         printEXPR (d+3) exp1
                                         putStrIdent (d+2) "To"
                                         printEXPR (d+3) exp2
                                         printBLOCK (d+4) block 