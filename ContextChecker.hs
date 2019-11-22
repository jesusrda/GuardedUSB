module ContextChecker where

import AST
import Control.Monad.State
import qualified Data.Map as H

-- Symbol
data Sym = VarSym {
    symID :: ID,
    symType :: TYPE
}

-- Symbolic table 
type SymTable = H.Map ID Sym

-- State
data OurState = OurState {
    tableStack :: [SymTable],
    bufferAST :: Either String [String]
}

-- State Monad
type StateM a = StateT OurState IO a 

-- Create empty SymTable
emptySymTable :: SymTable
emptySymTable = H.empty

-- Insert in SymTable
symTableInsert :: Sym -> SymTable -> SymTable
symTableInsert s = H.insert (symID s) s 

-- Lookup in SymTable
symTableLookup :: ID -> SymTable -> Maybe Sym
symTableLookup = H.lookup

-- Lookup for ID in current state
lookupID :: ID -> StateM (Maybe Sym)
lookupID id = do
    tStack <- gets tableStack
    return $ stackLookup tStack
    where
        stackLookup [] = Nothing
        stackLookup (t:ts) =
            case symTableLookup id t of
                Just s -> (Just s)
                Nothing -> stackLookup ts

stackPop :: StateM SymTable
stackPop = do
    (OurState (t:ts) buff) <- get
    put $ OurState ts buff
    return t

stackPush :: SymTable -> StateM ()
stackPush t = do
    (OurState tStack buffer) <- get
    put $ OurState (t : tStack) buffer

-- Auxiliar function to push an error to the state
printToError :: String -> StateM ()
printToError str = do
    (OurState tStack buffer) <- get
    case buffer of
        Left _ -> return ()
        Right _ -> put $ OurState tStack (Left ("Context Error: " ++ str))

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
    mapM_ (printToBuffer (d+1)) $ map printSym $ map snd $ H.toList t 
    printToBuffer 0 ""


checkTYPE :: Int -> (TYPE -> Bool) -> EXPR -> StateM ()
checkTYPE d f exp = do
    t <- traverseEXPR d exp
    if f t 
        then return ()
        else printToError "Type mismatch"

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

traverseDEC :: DECLARE -> StateM ()
traverseDEC dec = 
    case dec of
        (UNIQUETYPE ids tp) -> insertIDS ids $ replicate (length ids) tp
        (MULTITYPE ids tps) -> insertIDS ids tps
    where
        insertIDS [] [] = return ()
        insertIDS (id:ids) (tp:tps) = do
            st <- stackPop
            case symTableLookup id st of
                Nothing -> stackPush $ symTableInsert (VarSym id tp) st
                Just _ -> printToError $ "Variable: " ++ id ++ 
                                         " declared more than once in the same block"

traverseINSTS :: Int -> INSTRUCTIONS -> StateM ()
traverseINSTS d (INST inst) = traverseINST d inst
traverseINSTS d (SEQUENCE insts inst) = do
    printToBuffer d "Sequencing"
    traverseINSTS (d+1) insts
    traverseINST (d+1) inst

traverseINST :: Int -> INSTRUCTION -> StateM ()
traverseINST d (BLOCKINST block) = traverseBLOCK d block
traverseINST d (ASSIGNARRAY id exps) = do
    printToBuffer d "Assign Array"
    printToBuffer (d+1) $ "ID: " ++ id 
    sym <- lookupID id
    case sym of
        Nothing -> printToError $ "Variable " ++ id ++ " not in scope."
        Just s -> 
            if isARRAYL (length exps) (symType s)
                then return ()
                else printToError $ "Size mismatch in array " ++ id ++ "assignment"
    mapM_ (checkTYPE (d+1) isINT) exps
traverseINST d (ASSIGN id exp) = do
    printToBuffer d "Assign"
    printToBuffer (d+1) $ "ID: " ++ id
    sym <- lookupID id 
    case sym of
        Nothing -> printToError $ "Variable " ++ id ++ " not in scope."
        Just s -> checkTYPE (d+1) (\t -> t == symType s) exp
traverseINST d (READ id) = do
    printToBuffer d "Read"
    printToBuffer (d+1) $ "ID: " ++ id
    sym <- lookupID id
    case sym of
        Nothing -> printToError $ "Variable " ++ id ++ " not in scope."
        Just s -> return ()
traverseINST d (PRINT pexp) = do
    printToBuffer d "Print"
    traversePEXP (d+1) pexp
traverseINST d (PRINTLN pexp) = do 
    printToBuffer d "PrintLn"
    traversePEXP (d+1) pexp
traverseINST d (IFINST ifinst) = traverseIF ifinst
traverseINST d (DOINST doinst) = traverseDO doinst
traverseINST d (FORINST forinst) = traverseFOR forinst


traverseEXPR :: Int -> EXPR -> StateM TYPE
traverseEXPR d (SUM exp1 exp2) = do
    printToBuffer d "Plus"
    checkTYPE (d+1) isINT exp1
    checkTYPE (d+1) isINT exp2
    return INT
traverseEXPR d (MINUS exp1 exp2) = do
    printToBuffer d "Minus"
    checkTYPE (d+1) isINT exp1
    checkTYPE (d+1) isINT exp2
    return INT
traverseEXPR d (MULT exp1 exp2) = do
    printToBuffer d "Mult"
    checkTYPE (d+1) isINT exp1
    checkTYPE (d+1) isINT exp2
    return INT
traverseEXPR d (DIV exp1 exp2) = do
    printToBuffer d "Div"
    checkTYPE (d+1) isINT exp1
    checkTYPE (d+1) isINT exp2
    return INT
traverseEXPR d (MOD exp1 exp2) = do
    printToBuffer d "Mod"
    checkTYPE (d+1) isINT exp1
    checkTYPE (d+1) isINT exp2
    return INT
traverseEXPR d (ARRELEM exp1 exp2) = do
    printToBuffer d "ArrayElement"
    checkTYPE (d+1) isARRAY exp1
    checkTYPE (d+1) isINT exp2
    return INT
traverseEXPR d (AST.EQ exp1 exp2) = do
    printToBuffer d "Equal"
    t1 <- traverseEXPR (d+1) exp1
    t2 <- traverseEXPR (d+1) exp2
    case (t1, t2) of
        (INT, INT) -> return BOOL
        (BOOL, BOOL) -> return BOOL
        (_, _) -> do
            printToError "Type mismatch"
            return BOOL
traverseEXPR d (NEQ exp1 exp2) = do
    printToBuffer d "NotEqual"
    t1 <- traverseEXPR (d+1) exp1
    t2 <- traverseEXPR (d+1) exp2
    case (t1, t2) of
        (INT, INT) -> return BOOL
        (BOOL, BOOL) -> return BOOL
        (_, _) -> do
            printToError "Type mismatch"
            return BOOL
traverseEXPR d (LEQ exp1 exp2) = do
    printToBuffer d "LessEqual"
    checkTYPE (d+1) isINT exp1
    checkTYPE (d+1) isINT exp2
    return BOOL
traverseEXPR d (GEQ exp1 exp2) = do
    printToBuffer d "GreaterEqual"
    checkTYPE (d+1) isINT exp1
    checkTYPE (d+1) isINT exp2
    return BOOL
traverseEXPR d (LESS exp1 exp2) = do
    printToBuffer d "Less"
    checkTYPE (d+1) isINT exp1
    checkTYPE (d+1) isINT exp2
    return BOOL
traverseEXPR d (GREATER exp1 exp2) = do
    printToBuffer d "Greater"
    checkTYPE (d+1) isINT exp1
    checkTYPE (d+1) isINT exp2
    return BOOL
traverseEXPR d (AND exp1 exp2) = do
    printToBuffer d "And"
    checkTYPE (d+1) isBOOL exp1
    checkTYPE (d+1) isBOOL exp2
    return BOOL
traverseEXPR d (OR exp1 exp2) = do
    printToBuffer d "Or"
    checkTYPE (d+1) isBOOL exp1
    checkTYPE (d+1) isBOOL exp2
    return BOOL
traverseEXPR d (NOT exp) = do
    printToBuffer d "Not"
    checkTYPE (d+1) isBOOL exp
    return BOOL
traverseEXPR d (NEG exp) = do
    printToBuffer d "Negate"
    checkTYPE (d+1) isINT exp
    return INT
traverseEXPR d (ARRAYMOD exp1 exp2 exp3) = do
    printToBuffer d "ModifyArray"
    t <- traverseEXPR (d+1) exp1
    checkTYPE (d+1) isINT exp2
    checkTYPE (d+1) isINT exp3
    if isARRAY t
        then return t
        else do
            printToError "Type mismatch"
            return t
traverseEXPR d (SIZE exp) = do
    printToBuffer d "Size"
    checkTYPE (d+1) isARRAY exp
    return INT
traverseEXPR d (ATOI exp) = do
    printToBuffer d "Atoi"
    checkTYPE (d+1) (isARRAYL 1) exp
    return INT
traverseEXPR d (MIN exp) = do
    printToBuffer d "Min"
    checkTYPE (d+1) isARRAY exp
    return INT




traversePEXP = undefined

traverseIF = undefined

traverseDO = undefined

traverseFOR = undefined