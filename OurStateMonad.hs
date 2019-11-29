module OurStateMonad where

import AST
import SymTable
import Control.Monad.State

-- State
data OurState = OurState {
    tableStack :: [SymTable],
    bufferAST :: Either String [String]
}

-- State Monad
type StateM a = StateT OurState IO a 

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

-- Assign value to variable with ID assuming ID is in Scope
-- and there are no type conflicts
putValue :: ID -> SymValue -> StateM ()
putValue id val = do
    state <- get
    put $ state{tableStack = modifyStack $ tableStack state}
    where
        modifyStack [] = []
        modifyStack (t:ts) = 
            case symTableLookup id t of
                Just _ -> let newT = symTableModify id val t
                          in newT:ts 
                Nothing -> t:modifyStack ts 

stackPop :: StateM SymTable
stackPop = do
    (OurState (t:ts) buff) <- get
    put $ OurState ts buff
    return t

stackPush :: SymTable -> StateM ()
stackPush t = do
    (OurState tStack buffer) <- get
    put $ OurState (t : tStack) buffer