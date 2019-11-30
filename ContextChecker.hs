module ContextChecker where

import AST
import SymTable
import OurStateMonad
import Control.Monad.State

-- Auxiliar function to push an error to the state
printToError :: POS -> String -> StateM ()
printToError (l, c) str = do
    (OurState tStack buffer) <- get
    case buffer of
        Left _ -> return ()
        Right _ -> put $ OurState tStack (Left ("Context Error near line " ++ 
                                                 show l ++ " column " ++ show c ++ 
                                                 ": " ++ str))

-- Auxiliar function to print idented strings to buffer
printToBuffer :: Int -> String -> StateM ()
printToBuffer n str = do 
    let strIdent = (concat $ replicate n "  ") ++ str
    (OurState tStack buffer) <- get
    case buffer of
        Left _ -> return ()
        Right buff -> put $ OurState tStack (Right (strIdent:buff))

-- Type to String
printType :: TYPE -> String
printType INT = "int"
printType BOOL = "bool"
printType (ARRAY l r) = "array[" ++ (show l) ++ ".." ++ (show r) ++ "]"
printType FORVAR = "int (for)"

-- Symbol to String
printSym :: Sym -> String
printSym s = "variable: " ++ (symID s) ++ " | type: " ++ (printType $ symType s)

-- Print Current SymTable to buffer
printSymTable :: Int -> StateM ()
printSymTable d = do
    printToBuffer d "Symbols Table"
    (t:_) <- gets tableStack
    mapM_ (printToBuffer (d+1)) $ map printSym $ map snd $ symTableToList t 
    printToBuffer 0 ""

-- Checks if an expression is of the expected type 
-- and prints the expression or prints a Type mismatch error
checkTYPE :: POS -> Int -> (TYPE -> Bool) -> EXPR -> StateM ()
checkTYPE pos d f exp = do
    t <- traverseEXPR d exp
    if f t 
        then return ()
        else printToError pos "Type mismatch"

-- Checks if an array declaration is valid (l <= r)
-- and prints an error if not.
checkInvalidARRAY :: POS -> TYPE -> StateM ()
checkInvalidARRAY pos (ARRAY l r) 
    | r < l     = printToError pos "Invalid array declaration, left limit is greater than ritght limit"
    | otherwise = return ()
checkInvalidARRAY _ _ = return ()

-- Traverses the given Abstract Syntatic Tree and 
-- prints it with the respective Context Analysis
-- with Symbols Tables
traverseAST :: BLOCK -> StateM ()
traverseAST block = traverseBLOCK 0 block 

-- Function to traverse block
traverseBLOCK :: Int -> BLOCK -> StateM ()
traverseBLOCK d (BLOCK inst) = do
    printToBuffer d "Block"
    traverseINSTS (d+1) inst
traverseBLOCK d (BLOCKD decs inst) = do
    stackPush emptySymTable
    printToBuffer d "Block"
    traverseDECS decs
    printSymTable (d+1)
    traverseINSTS (d+1) inst
    stackPop
    return ()

-- Traverse declares inserting Symbols in SymTable
traverseDECS :: DECLARES -> StateM ()
traverseDECS (DECLARES dec) = traverseDEC dec
traverseDECS (SEQUENCED decs dec) = do
    traverseDECS decs
    traverseDEC dec

-- Traverse declare and checks for redeclaration of variables
traverseDEC :: DECLARE -> StateM ()
traverseDEC dec = 
    case dec of
        (UNIQUETYPE ids tp pos) -> insertIDS pos ids $ replicate (length ids) tp
        (MULTITYPE ids tps pos) -> insertIDS pos ids (reverse tps)
    where
        insertIDS pos [] [] = return ()
        insertIDS pos (id:ids) (tp:tps) = do
            st <- stackPop
            checkInvalidARRAY pos tp
            case symTableLookup id st of
                Nothing -> do
                    stackPush $ symTableInsert (VarSym id tp Nothing) st
                    insertIDS pos ids tps
                Just _ -> printToError pos $ "Variable: '" ++ id ++ 
                                             "' declared more than once in the same block"

-- Traverse instructions
traverseINSTS :: Int -> INSTRUCTIONS -> StateM ()
traverseINSTS d (INST inst) = traverseINST d inst
traverseINSTS d (SEQUENCE insts inst) = do
    printToBuffer d "Sequencing"
    traverseINSTS (d+1) insts
    traverseINST (d+1) inst

-- Traverse instruction
traverseINST :: Int -> INSTRUCTION -> StateM ()
traverseINST d (BLOCKINST block) = traverseBLOCK d block
traverseINST d (ASSIGNARRAY id exps pos) = do
    printToBuffer d "Assign Array"
    printToBuffer (d+1) $ "ID: " ++ id 
    sym <- lookupID id
    case sym of
        Nothing -> printToError pos $ "Variable '" ++ id ++ "' not in scope."
        Just s -> 
            if isARRAYL (length exps) (symType s)
                then return ()
                else printToError pos $ "Size mismatch in array '" ++ id ++ "' assignment"
    mapM_ (checkTYPE pos (d+1) isINT) exps
traverseINST d (ASSIGN id exp pos) = do
    printToBuffer d "Assign"
    printToBuffer (d+1) $ "ID: " ++ id
    sym <- lookupID id 
    case sym of
        Nothing -> printToError pos $ "Variable " ++ id ++ " not in scope."
        Just s -> checkTYPE pos (d+1) (\t -> t == symType s || (isINT t && isARRAYL 1 (symType s))) exp
traverseINST d (READ id pos) = do
    printToBuffer d "Read"
    printToBuffer (d+1) $ "ID: " ++ id
    sym <- lookupID id
    case sym of
        Nothing -> printToError pos $ "Variable " ++ id ++ " not in scope."
        Just s -> return ()
traverseINST d (PRINT pexp pos) = do
    printToBuffer d "Print"
    traversePEXP (d+1) pexp
traverseINST d (PRINTLN pexp pos) = do 
    printToBuffer d "PrintLn"
    traversePEXP (d+1) pexp 
traverseINST d (IFINST ifinst) = traverseIF d ifinst
traverseINST d (DOINST doinst) = traverseDO d doinst
traverseINST d (FORINST forinst) = traverseFOR d forinst

-- Traverse Expression and returns its type (in the StateM monad)
traverseEXPR :: Int -> EXPR -> StateM TYPE
traverseEXPR d (SUM exp1 exp2 pos) = do
    printToBuffer d "Plus"
    checkTYPE pos (d+1) isINT exp1
    checkTYPE pos (d+1) isINT exp2
    return INT
traverseEXPR d (MINUS exp1 exp2 pos) = do
    printToBuffer d "Minus"
    checkTYPE pos (d+1) isINT exp1
    checkTYPE pos (d+1) isINT exp2
    return INT
traverseEXPR d (MULT exp1 exp2 pos) = do
    printToBuffer d "Mult"
    checkTYPE pos (d+1) isINT exp1
    checkTYPE pos (d+1) isINT exp2
    return INT
traverseEXPR d (DIV exp1 exp2 pos) = do
    printToBuffer d "Div"
    checkTYPE pos (d+1) isINT exp1
    checkTYPE pos (d+1) isINT exp2
    return INT
traverseEXPR d (MOD exp1 exp2 pos) = do
    printToBuffer d "Mod"
    checkTYPE pos (d+1) isINT exp1
    checkTYPE pos (d+1) isINT exp2
    return INT
traverseEXPR d (ARRELEM exp1 exp2 pos) = do
    printToBuffer d "ArrayElement"
    checkTYPE pos (d+1) isARRAY exp1
    checkTYPE pos (d+1) isINT exp2
    return INT
traverseEXPR d (AST.EQ exp1 exp2 pos) = do
    printToBuffer d "Equal"
    t1 <- traverseEXPR (d+1) exp1
    t2 <- traverseEXPR (d+1) exp2
    case (t1, t2) of
        (INT, INT) -> return BOOL
        (BOOL, BOOL) -> return BOOL
        (_, _) -> do
            printToError pos "Type mismatch"
            return BOOL
traverseEXPR d (NEQ exp1 exp2 pos) = do
    printToBuffer d "NotEqual"
    t1 <- traverseEXPR (d+1) exp1
    t2 <- traverseEXPR (d+1) exp2
    case (t1, t2) of
        (INT, INT) -> return BOOL
        (BOOL, BOOL) -> return BOOL
        (_, _) -> do
            printToError pos "Type mismatch"
            return BOOL
traverseEXPR d (LEQ exp1 exp2 pos) = do
    printToBuffer d "LessEqual"
    checkTYPE pos (d+1) isINT exp1
    checkTYPE pos (d+1) isINT exp2
    return BOOL
traverseEXPR d (GEQ exp1 exp2 pos) = do
    printToBuffer d "GreaterEqual"
    checkTYPE pos (d+1) isINT exp1
    checkTYPE pos (d+1) isINT exp2
    return BOOL
traverseEXPR d (LESS exp1 exp2 pos) = do
    printToBuffer d "Less"
    checkTYPE pos (d+1) isINT exp1
    checkTYPE pos (d+1) isINT exp2
    return BOOL
traverseEXPR d (GREATER exp1 exp2 pos) = do
    printToBuffer d "Greater"
    checkTYPE pos (d+1) isINT exp1
    checkTYPE pos (d+1) isINT exp2
    return BOOL
traverseEXPR d (AND exp1 exp2 pos) = do
    printToBuffer d "And"
    checkTYPE pos (d+1) isBOOL exp1
    checkTYPE pos (d+1) isBOOL exp2
    return BOOL
traverseEXPR d (OR exp1 exp2 pos) = do
    printToBuffer d "Or"
    checkTYPE pos (d+1) isBOOL exp1
    checkTYPE pos (d+1) isBOOL exp2
    return BOOL
traverseEXPR d (NOT exp pos) = do
    printToBuffer d "Not"
    checkTYPE pos (d+1) isBOOL exp
    return BOOL
traverseEXPR d (NEG exp pos) = do
    printToBuffer d "Negate"
    checkTYPE pos (d+1) isINT exp
    return INT
traverseEXPR d (ARRAYMOD exp1 exp2 exp3 pos) = do
    printToBuffer d "ModifyArray"
    t <- traverseEXPR (d+1) exp1
    checkTYPE pos (d+1) isINT exp2
    checkTYPE pos (d+1) isINT exp3
    if isARRAY t
        then return t
        else do
            printToError pos "Type mismatch"
            return t
traverseEXPR d (SIZE exp pos) = do
    printToBuffer d "Size"
    checkTYPE pos (d+1) isARRAY exp
    return INT
traverseEXPR d (ATOI exp pos) = do
    printToBuffer d "Atoi"
    checkTYPE pos (d+1) (isARRAYL 1) exp
    return INT
traverseEXPR d (MIN exp pos) = do
    printToBuffer d "Min"
    checkTYPE pos (d+1) isARRAY exp
    return INT
traverseEXPR d (MAX exp pos) = do
    printToBuffer d "Max"
    checkTYPE pos (d+1) isARRAY exp
    return INT
traverseEXPR d (IDT id pos) = do
    printToBuffer d $ "ID: " ++ id
    sym <- lookupID id
    case sym of
        Just s -> return $ 
            case symType s of
                FORVAR -> INT
                t -> t 
        Nothing -> do
            printToError pos $ "Variable '" ++ id ++ "' not in scope."
            return INT 
traverseEXPR d TRUE = do
    printToBuffer d $ "True"
    return BOOL
traverseEXPR d FALSE = do
    printToBuffer d $ "False"
    return BOOL
traverseEXPR d (NUM n) = do
    printToBuffer d $ "Literal: " ++ show n
    return INT

-- Traverse printable expression
traversePEXP :: Int -> PRINTEXP -> StateM ()
traversePEXP d (CONCAT exp1 exp2) = do
    printToBuffer d "Concat"
    traversePEXP (d+1) exp1
    traversePEXP (d+1) exp2
traversePEXP d (PEXPR exp) = do
    traverseEXPR d exp
    return ()
traversePEXP d (STRINGLIT s) = 
    printToBuffer d $ "\"" ++ s ++ "\"" 

-- Traverse IF node
traverseIF :: Int -> IF -> StateM ()
traverseIF d (IF gs) = do
    printToBuffer d "If"
    traverseGUARDS (d+1) gs

-- Traverse DO node
traverseDO :: Int -> DO -> StateM ()
traverseDO d (DO gs) = do
    printToBuffer d "Do"
    traverseGUARDS (d+1) gs

-- Traverse Guards for Do or If expression
traverseGUARDS :: Int -> GUARDS -> StateM ()
traverseGUARDS d (GUARDS exp insts pos) = do
    printToBuffer d "Guard"
    checkTYPE pos (d+1) isBOOL exp -- En caso de no ser bool deberiamos imprimir la posición del IF
    traverseINSTS (d+1) insts
traverseGUARDS d (GUARDSEQ g1 g2) = do
    traverseGUARDS d g1
    traverseGUARDS d g2

-- Traverse FOR node
traverseFOR :: Int -> FOR -> StateM ()
traverseFOR d (FOR id exp1 exp2 block pos) = do
    printToBuffer d "For"
    printToBuffer (d+1) $ "ID: " ++ id
    printToBuffer (d+2) "In"
    checkTYPE pos (d+3) isINT exp1
    printToBuffer (d+2) "To"
    checkTYPE pos (d+3) isINT exp2
    stackPush $ symTableInsert (VarSym id FORVAR Nothing) emptySymTable
    printSymTable (d+4)
    traverseBLOCK (d+4) block 