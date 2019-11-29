module SymTable where

import AST
import qualified Data.Map as H
import Data.Array

-- Value
data SymValue = IntValue Int | BoolValue Bool | ArrayValue (Array Int Int) 

-- Symbol
data Sym = VarSym {
    symID :: ID,
    symType :: TYPE,
    symValue :: Maybe SymValue
}

-- Symbolic table 
type SymTable = H.Map ID Sym

-- Create empty SymTable
emptySymTable :: SymTable
emptySymTable = H.empty

-- Insert in SymTable
symTableInsert :: Sym -> SymTable -> SymTable
symTableInsert s = H.insert (symID s) s 

-- Lookup in SymTable
symTableLookup :: ID -> SymTable -> Maybe Sym
symTableLookup = H.lookup

-- Modify element with specified id
symTableModify :: ID -> SymValue -> SymTable -> SymTable
symTableModify id val = H.adjust (\s -> s{symValue = Just val}) id

-- Convert symbols table to list
symTableToList :: SymTable -> [(ID, Sym)]
symTableToList = H.toList

getIntVal :: SymValue -> Int 
getIntVal (IntValue n) = n
getIntVal _ = error "Trying to get int value from non IntValue constructor"