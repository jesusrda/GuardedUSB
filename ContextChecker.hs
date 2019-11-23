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
    mapM_ (printToBuffer (d+1)) $ map printSym $ map snd $ H.toList t 
    printToBuffer 0 ""


checkTYPE :: POS -> Int -> (TYPE -> Bool) -> EXPR -> StateM ()
checkTYPE pos d f exp = do
    t <- traverseEXPR pos d exp
    if f t 
        then return ()
        else printToError pos "Type mismatch"

checkInvalidARRAY :: POS -> TYPE -> StateM ()
checkInvalidARRAY pos (ARRAY l r) 
    | r < l     = printToError pos "Invalid array declaration, left limit is greater than ritght limit"
    | otherwise = return ()
checkInvalidARRAY _ _ = return ()

traverseAST :: BLOCK -> StateM ()
traverseAST block = do
    traverseBLOCK 0 block 
    buffer <- gets bufferAST
    case buffer of
        Left err -> liftIO $ print err
        Right lns -> liftIO $ putStr $ unlines $ reverse lns
    return ()

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
        (UNIQUETYPE ids tp pos) -> insertIDS pos ids $ replicate (length ids) tp
        (MULTITYPE ids tps pos) -> insertIDS pos ids tps
    where
        insertIDS pos [] [] = return ()
        insertIDS pos (id:ids) (tp:tps) = do
            st <- stackPop
            checkInvalidARRAY pos tp
            case symTableLookup id st of
                Nothing -> do
                    stackPush $ symTableInsert (VarSym id tp) st
                    insertIDS pos ids tps
                Just _ -> printToError pos $ "Variable: '" ++ id ++ 
                                             "' declared more than once in the same block"

traverseINSTS :: Int -> INSTRUCTIONS -> StateM ()
traverseINSTS d (INST inst) = traverseINST d inst
traverseINSTS d (SEQUENCE insts inst) = do
    printToBuffer d "Sequencing"
    traverseINSTS (d+1) insts
    traverseINST (d+1) inst

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
        Just s -> checkTYPE pos (d+1) (\t -> t == symType s) exp
traverseINST d (READ id pos) = do
    printToBuffer d "Read"
    printToBuffer (d+1) $ "ID: " ++ id
    sym <- lookupID id
    case sym of
        Nothing -> printToError pos $ "Variable " ++ id ++ " not in scope."
        Just s -> return ()
traverseINST d (PRINT pexp pos) = do
    printToBuffer d "Print"
    traversePEXP pos (d+1) pexp
traverseINST d (PRINTLN pexp pos) = do 
    printToBuffer d "PrintLn"
    traversePEXP pos (d+1) pexp 
traverseINST d (IFINST ifinst) = traverseIF d ifinst
traverseINST d (DOINST doinst) = traverseDO d doinst
traverseINST d (FORINST forinst) = traverseFOR d forinst


traverseEXPR :: POS -> Int -> EXPR -> StateM TYPE
traverseEXPR _ d (SUM exp1 exp2 pos) = do
    printToBuffer d "Plus"
    checkTYPE pos (d+1) isINT exp1
    checkTYPE pos (d+1) isINT exp2
    return INT
traverseEXPR _ d (MINUS exp1 exp2 pos) = do
    printToBuffer d "Minus"
    checkTYPE pos (d+1) isINT exp1
    checkTYPE pos (d+1) isINT exp2
    return INT
traverseEXPR _ d (MULT exp1 exp2 pos) = do
    printToBuffer d "Mult"
    checkTYPE pos (d+1) isINT exp1
    checkTYPE pos (d+1) isINT exp2
    return INT
traverseEXPR _ d (DIV exp1 exp2 pos) = do
    printToBuffer d "Div"
    checkTYPE pos (d+1) isINT exp1
    checkTYPE pos (d+1) isINT exp2
    return INT
traverseEXPR _ d (MOD exp1 exp2 pos) = do
    printToBuffer d "Mod"
    checkTYPE pos (d+1) isINT exp1
    checkTYPE pos (d+1) isINT exp2
    return INT
traverseEXPR _ d (ARRELEM exp1 exp2 pos) = do
    printToBuffer d "ArrayElement"
    checkTYPE pos (d+1) isARRAY exp1
    checkTYPE pos (d+1) isINT exp2
    return INT
traverseEXPR _ d (AST.EQ exp1 exp2 pos) = do
    printToBuffer d "Equal"
    t1 <- traverseEXPR pos (d+1) exp1
    t2 <- traverseEXPR pos (d+1) exp2
    case (t1, t2) of
        (INT, INT) -> return BOOL
        (BOOL, BOOL) -> return BOOL
        (_, _) -> do
            printToError pos "Type mismatch"
            return BOOL
traverseEXPR _ d (NEQ exp1 exp2 pos) = do
    printToBuffer d "NotEqual"
    t1 <- traverseEXPR pos (d+1) exp1
    t2 <- traverseEXPR pos (d+1) exp2
    case (t1, t2) of
        (INT, INT) -> return BOOL
        (BOOL, BOOL) -> return BOOL
        (_, _) -> do
            printToError pos "Type mismatch"
            return BOOL
traverseEXPR _ d (LEQ exp1 exp2 pos) = do
    printToBuffer d "LessEqual"
    checkTYPE pos (d+1) isINT exp1
    checkTYPE pos (d+1) isINT exp2
    return BOOL
traverseEXPR _ d (GEQ exp1 exp2 pos) = do
    printToBuffer d "GreaterEqual"
    checkTYPE pos (d+1) isINT exp1
    checkTYPE pos (d+1) isINT exp2
    return BOOL
traverseEXPR _ d (LESS exp1 exp2 pos) = do
    printToBuffer d "Less"
    checkTYPE pos (d+1) isINT exp1
    checkTYPE pos (d+1) isINT exp2
    return BOOL
traverseEXPR _ d (GREATER exp1 exp2 pos) = do
    printToBuffer d "Greater"
    checkTYPE pos (d+1) isINT exp1
    checkTYPE pos (d+1) isINT exp2
    return BOOL
traverseEXPR _ d (AND exp1 exp2 pos) = do
    printToBuffer d "And"
    checkTYPE pos (d+1) isBOOL exp1
    checkTYPE pos (d+1) isBOOL exp2
    return BOOL
traverseEXPR _ d (OR exp1 exp2 pos) = do
    printToBuffer d "Or"
    checkTYPE pos (d+1) isBOOL exp1
    checkTYPE pos (d+1) isBOOL exp2
    return BOOL
traverseEXPR _ d (NOT exp pos) = do
    printToBuffer d "Not"
    checkTYPE pos (d+1) isBOOL exp
    return BOOL
traverseEXPR _ d (NEG exp pos) = do
    printToBuffer d "Negate"
    checkTYPE pos (d+1) isINT exp
    return INT
traverseEXPR _ d (ARRAYMOD exp1 exp2 exp3 pos) = do
    printToBuffer d "ModifyArray"
    t <- traverseEXPR pos (d+1) exp1
    checkTYPE pos (d+1) isINT exp2
    checkTYPE pos (d+1) isINT exp3
    if isARRAY t
        then return t
        else do
            printToError pos "Type mismatch"
            return t
traverseEXPR _ d (SIZE exp pos) = do
    printToBuffer d "Size"
    checkTYPE pos (d+1) isARRAY exp
    return INT
traverseEXPR _ d (ATOI exp pos) = do
    printToBuffer d "Atoi"
    checkTYPE pos (d+1) (isARRAYL 1) exp
    return INT
traverseEXPR _ d (MIN exp pos) = do
    printToBuffer d "Min"
    checkTYPE pos (d+1) isARRAY exp
    return INT
traverseEXPR _ d (MAX exp pos) = do
    printToBuffer d "Max"
    checkTYPE pos (d+1) isARRAY exp
    return INT
traverseEXPR pos d (IDT id) = do
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
traverseEXPR _ d TRUE = do
    printToBuffer d $ "True"
    return BOOL
traverseEXPR _ d FALSE = do
    printToBuffer d $ "False"
    return BOOL
traverseEXPR _ d (NUM n) = do
    printToBuffer d $ "Literal: " ++ show n
    return INT

traversePEXP :: POS -> Int -> PRINTEXP -> StateM ()
traversePEXP pos d (CONCAT exp1 exp2) = do
    printToBuffer d "Concat"
    traversePEXP pos (d+1) exp1
    traversePEXP pos (d+1) exp2
traversePEXP pos d (PEXPR exp) = do
    traverseEXPR pos d exp
    return ()
traversePEXP _ d (STRINGLIT s) = 
    printToBuffer d $ "\"" ++ s ++ "\"" 

traverseIF :: Int -> IF -> StateM ()
traverseIF d (IF gs) = do
    printToBuffer d "If"
    traverseGUARDS (d+1) gs

traverseDO :: Int -> DO -> StateM ()
traverseDO d (DO gs) = do
    printToBuffer d "Do"
    traverseGUARDS (d+1) gs

traverseGUARDS :: Int -> GUARDS -> StateM ()
traverseGUARDS d (GUARDS exp insts pos) = do
    printToBuffer d "Guard"
    checkTYPE pos (d+1) isBOOL exp -- En caso de no ser bool deberiamos imprimir la posición del IF
    traverseINSTS (d+1) insts
traverseGUARDS d (GUARDSEQ g1 g2) = do
    traverseGUARDS d g1
    traverseGUARDS d g2

traverseFOR :: Int -> FOR -> StateM ()
traverseFOR d (FOR id exp1 exp2 block pos) = do
    printToBuffer d "For"
    printToBuffer (d+1) $ "ID: " ++ id
    printToBuffer (d+2) "In"
    checkTYPE pos (d+3) isINT exp1
    printToBuffer (d+2) "To"
    checkTYPE pos (d+3) isINT exp2
    stackPush $ symTableInsert (VarSym id INT) emptySymTable
    printSymTable (d+4)
    traverseBLOCK (d+4) block 