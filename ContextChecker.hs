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

stackPop :: StateM ()
stackPop = do
    (OurState (t:ts) buff) <- get
    put $ OurState ts buff

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
    (OurState tStack buffer) <- get
    put $ OurState (emptySymTable : tStack) buffer
    printToBuffer d "Block"
    traverseDECS decs
    printSymTable (d+1)
    traverseINSTS (d+1) inst
    stackPop

-- Traverse thr
traverseDECS :: DECLARES -> StateM ()
traverseDECS = undefined


traverseINSTS = undefined