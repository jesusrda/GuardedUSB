module RunAST  where

import AST
import SymTable 
import OurStateMonad
import Control.Monad.State
import ContextChecker (traverseDECS)
import System.Exit
import Data.Array
import Text.Read 

printError :: POS -> String -> StateM ()
printError (l, c) str = do 
    liftIO $ putStrLn $ "\nException thrown near line " ++
                        show l ++ " column " ++ show c ++ ": " ++ str
    liftIO $ exitSuccess
readMaybeBool :: String -> Maybe Bool
readMaybeBool "true" = Just True
readMaybeBool "false" = Just False
readMaybeBool _ = Nothing 

readOfType :: POS -> TYPE -> StateM (Maybe SymValue)
readOfType pos INT = do
    inp <- liftIO getLine
    let val = readMaybe inp :: Maybe Int 
    case val of
        Just v -> return $ IntValue v
        Nothing -> do 
            printError pos "Invalid input value"
            return Nothing
readOfType pos BOOL = do
    inp <- liftIO getLine
    let val = readMaybeBool inp 
    case val of
        Just v -> return $ BoolValue v
        Nothing -> do 
            printError pos "Invalid input value"
            return Nothing
readOfType pos (ARRAY l r) = do
    inp <- liftIO getLine
    let val = readMaybe ("[" ++ inp ++ "]") :: [Int]
    case val of 
        Just v -> do
            if length v == r - l + 1 
                then return $ Just $ ArrayValue $ listArray (l, r) v 
                else do
                    printError pos "Invalid input value (array lenght doesn't match)"
                    return Nothing
        Nothing -> do
            printError pos "Invalid input value"
            return Nothing

runBLOCK :: BLOCK -> StateM ()
runBLOCK (BLOCK inst) = runINSTS inst
runBLOCK (BLOCKD decs inst) = do
    stackPush emptySymTable
    traverseDECS decs 
    runINSTS inst 
    stackPop
    return ()

runINSTS :: INSTRUCTIONS -> StateM ()
runINSTS (INST inst) = runINST inst
runINSTS (SEQUENCE insts inst) = do
    runINSTS insts
    runINST inst    

runINST :: INSTRUCTION -> StateM ()
runINST (BLOCKINST block) = runBLOCK
runINST (ASSIGNARRAY id exps _) = do
    expsVal <- mapM $ runEXPR exps
    sym <- lookupID id
    case sym of
        Just s -> putValue id $ 
            ArrayValue $ listArray (getLimits $ symType s) (map getIntVal expsVal)
        Nothing -> return ()
runINST (ASSIGN id exp _) = do
    expVal <- runEXPR exp
    putValue id expVal 
runINST (READ id pos) = do
    sym <- lookupID id
    case sym of 
        Nothing -> return ()
        Just s -> do
            val <- readOfType pos (symType s)
            case val of 
                Just v -> putValue id v
                Nothing -> return ()
runINST (PRINT pexp _) = do
    str <- runPEXP pexp
    liftIO $ putStr str
    return () 
runINST (PRINTLN pexp _) = do
    str <- runPEXP pexp
    liftIO $ putStrLn str
    return () 
runINST (IFINST ifinst) = runIF ifinst
runINST (DOINST doinst) = runDO doinst
runINST (FORINST forinst) = runFOR forinst

runEXPR :: EXPR -> StateM SymValue
runEXPR (SUM exp1 exp2 _) = do
    val1 <- runEXPR exp1
    val2 <- runEXPR exp2
    return $ IntValue $ getIntVal val1 + getIntVal val2
runEXPR (MINUS exp1 exp2 _) = do
    val1 <- runEXPR exp1
    val2 <- runEXPR exp2
    return $ IntValue $ getIntVal val1 - getIntVal val2 
runEXPR (MULT exp1 exp2 _) = do
    val1 <- runEXPR exp1
    val2 <- runEXPR exp2
    return $ IntValue $ getIntVal val1 * getIntVal val2
runEXPR (DIV exp1 exp2 pos) = do
    val1 <- runEXPR exp1
    val2 <- runEXPR exp2
    case getIntVal val2 of
        0 -> do
            printError pos "Division by zero"
            return $ IntValue 0 -- Estaría bien poner un NULL aquí
        n -> return $ IntValue $ getIntVal $ val1 'div' n
runEXPR (MOD exp1 exp2 pos) = do
    val1 <- runEXPR exp1
    val2 <- runEXPR exp2
    case getIntVal val2 of
        0 -> do
            printError pos "Division by zero"
            return $ IntValue 0 -- Estaría bien poner un NULL aquí
        n -> return $ Intvalue $ getIntVal $ val1 'mod' n
runEXPR (ARRELEM exp1 exp2 pos) = do
    symArr <- runEXPR exp1
    symIdx <- runExpr exp2
    let arr = getArrayVal symArr
        idx = getIntVal symIdx
        (lo, up) = bounds arr in 
        if lo <= idx && idx <= up then do
            return $ IntValue $ arr ! idx
        else do
            printError pos "Accessing array out of bounds"
            return $ IntValue 0 -- Estaría bien poner un NULL aquí
runEXPR (AST.EQ exp1 exp2 _) = do
    val1 <- runEXPR exp1
    val2 <- runEXPR exp2
    case (val1, val2) of
        (BoolValue b1, BoolValue b2) -> do
            return $ BoolValue $ b1 == b2
        (IntValue n1, IntValue n2) -> do
            return $ BoolValue $ n1 == n2
runEXPR (NEQ exp1 exp2 _) = do
    val1 <- runEXPR exp1
    val2 <- runEXPR exp2
    case (val1, val2) of
        (BoolValue b1, BoolValue b2) -> do
            return $ BoolValue $ b1 /= b2
        (IntValue n1, IntValue n2) -> do
            return $ BoolValue $ n1 /= n2 
runEXPR (LEQ exp1 exp2 _) = do
    val1 <- runEXPR exp1
    val2 <- runEXPR exp2
    return $ BoolValue $ getIntVal val1 <= getIntVal val2
runEXPR (GEQ exp1 exp2 _) = do
    val1 <- runEXPR exp1
    val2 <- runEXPR exp2
    return $ BoolValue $ getIntVal val1 >= getIntVal val2
runEXPR (LESS exp1 exp2 _) = do
    val1 <- runEXPR exp1
    val2 <- runEXPR exp2
    return $ BoolValue $ getIntVal val1 < getIntVal val2
runEXPR (GREATER exp1 exp2 _) = do
    val1 <- runEXPR exp1
    val2 <- runEXPR exp2
    return $ BoolValue $ getIntVal val1 > getIntVal val2
runEXPR (AND exp1 exp2 _) = do
    val1 <- runEXPR exp1
    val2 <- runEXPR exp2
    return $ BoolValue $ getBoolVal val1 && getBoolVal val2
runEXPR (OR exp1 exp2 _) = do
    val1 <- runEXPR exp1
    val2 <- runEXPR exp2
    return $ BoolValue $ getBoolVal val1 || getBoolVal val2
runEXPR (NOT exp _) = do
    val <- runEXPR exp
    return $ BoolValue $ not (getBoolVal val)
runEXPR (NEG exp _) = do
    val <- runEXPR exp
    return $ IntValue $ - (getIntValue val)
runEXPR (ARRAYMOD exp1 exp2 exp3 _) = do
    symArr <- runEXPR exp1
    symIdx <- runEXPR exp2
    symInt <- runEXPR exp3
    let arr = getArrayVal symArr
        idx = getIntVal symIdx
        val = getIntVal symInt
        (lo, up) = bounds arr in
        if lo<=idx && idx<=hi then do
            return $ ArrayValue $ arr//[(idx,val)]
        else do
            printError pos "Accessing array out of bounds"
            return $ IntValue 0 -- AQUI TAMBIEN LO DE LOS NULL
runEXPR (SIZE exp _) = do
    symArr <- runEXPR exp
    let (lo, up) = bounds $ getArrayVal symArr in
        return $ IntValue (up-lo+1)
runEXPR (ATOI exp _) = do
    symArr <- runEXPR exp
    let arr = getArrayVal symArr
        (lo, up) = bounds arr in
        return $ IntValue (arr!lo)
runEXPR (MIN exp _) = do
    symArr <- runEXPR exp
    let (lo, up) = bounds $ getArrayVal symArr in
        return $ IntValue lo
runEXPR (MAX exp _) = do
    symArr <- runEXPR exp
    let (lo, up) = bounds $ getArrayVal symArr in
        return $ IntValue up
runEXPR (IDT id pos) = do
    sym <- lookupID id
    let (Just s) = sym in
        case symValue s of
            Just sVal -> do
                return sVal      
            Nothing -> do
                printError pos "Attempting to use uninitialized variable"
                return $ IntValue 0 -- Aqui tambien jejeps
runEXPR TRUE = return $ BoolValue True
runEXPR FALSE = return $ BoolValue False
runEXPR (NUM n) = return $ IntValue n

runPEXP :: EXPR -> StateM String
runPEXP expr = do
    val <- runEXPR expr
    case val of 
        IntValue i -> return $ show i
        BoolValue b -> return $ show b
        ArrayValue ar -> return $ showArray (fst (bounds ar)) (elems ar)
    where
        showArray _ [a] = (show n) ++ ":" ++ (show a)
        showArray n (a:as) = (show n) ++ ":" ++ (show a) ++ ", " ++ (showArray as)

runIF :: IF -> StateM ()
runIF g = do
    b <- runGUARDS g
    return ()

runDO :: DO -> StateM ()
runDO g = do
    b <- runGUARDS g
    if b 
        then do
            runDO g
        else
            return ()
            
runGUARDS :: GUARDS -> StateM Bool
runGUARDS (GUARDS exp insts _) = do
    cond <- runEXPR exp 
    if getBoolVal cond 
        then do 
            runINSTS insts 
            return True 
        else return False 
runGUARDS (GUARDSEQ g1 g2) = do
    done <- runGUARDS g1
    if done 
        then return True 
        else runGUARDS g2

runFOR :: FOR -> StateM ()
runFOR (FOR id exp1 exp2 bl _) = do
    i <- getIntVal $ runEXPR exp1
    j <- getIntVal $ runEXPR exp2
    stackPush $ symTableInsert (VarSym id FORVAR Nothing) emptySymTable
    loop bl i j id
    stackPop
    return ()


loop :: BLOCK -> Int -> Int -> String -> StateM ()
loop bl i j id
    | i > j = return ()
    | otherwise = do
        putValue id $ IntValue i
        runBLOCK bl
        loop bl (i+1) j id