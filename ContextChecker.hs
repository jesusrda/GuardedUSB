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
    sym <- lookupID id
    case sym of
        Nothing -> printToError $ "Variable " ++ id ++ " not in scope."
        Just s ->
            case symType s of
                ARRAY l r -> if (r - l) == (length exps) 
                                then printToBuffer (d+1) ("ID: " ++ id)
                                else printToError $ "Size of array " ++ id ++ 
                                                    " differ from size of expression list"
                _ -> printToError $ "Type mismatch: variable " ++ id ++ "is not an array" 
    ts <- mapM (traverseEXPR (d+1)) exps
    if (all (== INT) ts) 
        then return ()
        else printToError $ "Some expressions are not integers in " ++ id ++ " array asignment"
traverseINST d (ASSIGN id exp) = do
    printToBuffer d "Assign"
    printToBuffer (d+1) ("ID: " ++ id)
    t <- traverseEXPR (d+1) exp
    sym <- lookupID id 
    case sym of
        Nothing -> printToError $ "Variable " ++ id ++ " not in scope."
        Just s -> 
            if (symType s == t) 
                then return ()
                else printToError $ "Type Mismatch in variable " ++ id ++ " assignment"
traverseINST d (READ id) = do
    printToBuffer d "Read"
    printToBuffer (d+1) ("ID: " ++ id)
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
    t1 <- traverseEXPR (d+1) exp1
    t2 <- traverseEXPR (d+1) exp2
    return t1

traversePEXP = undefined

traverseIF = undefined

traverseDO = undefined

traverseFOR = undefined