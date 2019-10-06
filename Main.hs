import System.Environment
import Data.List
import Lexer

main = do
    f <- getArgs >>= return . head
    if isSuffixOf ".gusb" f
    then do
	    s <- readFile f
	    case scanner s of
	        Left s -> putStr s
	        Right toks -> putStr $ unlines $ map showTokenPos toks
	else putStrLn "El archivo dado no tiene la terminacion correcta."