import System.Environment
import Data.List
import Lexer
import Parser
import AST
import ContextChecker
import Control.Monad.State
import OurStateMonad

main = do
    f <- getArgs >>= return . head
    if isSuffixOf ".gusb" f
    then do
	    s <- readFile f
	    case scanner s of
	        Left s -> putStr s
	        Right toks -> do
                runStateT (traverseAST $ parse toks) (OurState [] (Right []))
                return ()
	else putStrLn "Error: Wrong Filetype. Only .gusb files allowed"