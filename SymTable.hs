module SymTable where

import AST
import qualified Data.Map as H
import Data.Array

-- Value
data Value = IntValue Int | BoolValue Bool | ArrayValue (Array Int Int)

-- Symbol
data Sym = VarSym {
    symID :: ID,
    symType :: TYPE,
    symValue :: Maybe Value
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

-- Convert symbols table to list
symTableToList :: SymTable -> [(ID, Sym)]
symTableToList = H.toList