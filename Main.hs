import Lexer

main = do
    f <- getLine
    s <- readFile f
    case scanner s of
        Left s -> putStr s
        Right toks -> putStr $ unlines $ map showTokenPos toks