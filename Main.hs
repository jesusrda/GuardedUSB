import System.Environment
import Data.List
import Lexer
import Parser
import AST

main = do
    f <- getArgs >>= return . head
    if isSuffixOf ".gusb" f
    then do
	    s <- readFile f
	    case scanner s of
	        Left s -> putStr s
	        Right toks -> printAST $ parse toks
	else putStrLn "Error: Wrong Filetype. Only .gusb files allowed"