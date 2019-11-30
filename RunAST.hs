module RunAST  where

import AST
import SymTable 
import OurStateMonad
import Control.Monad.State
import ContextChecker (traverseDECS)
import System.Exit
import Data.Array

printError :: POS -> String -> StateM ()
printError (l, c) str = do 
    liftIO $ putStrLn $ "\nException thrown near line " ++
                        show l ++ " column " ++ show c ++ ": " ++ str
    liftIO $ exitSuccess

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
    inp <- liftIO getLine
    sym <- lookupID id

runEXPR :: EXPR -> StateM SymValue
runEXPR = undefined

runPEXP :: EXPR -> StateM String
runPEXP s = undefined

runIF :: IF -> StateM ()
runIF g = undefined

runDO :: DO -> StateM ()
runDO = undefined 

runGUARDS :: GUARDS -> StateM Bool
runGUARDS = undefined

runFOR :: FOR -> StateM ()
runFOR = undefined