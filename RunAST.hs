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
runEXPR = undefined

runPEXP :: EXPR -> StateM String
runPEXP = undefined

runIF :: IF -> StateM ()
runIF = undefined

runDO :: DO -> StateM ()
runDO = undefined 

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
runFOR = undefined