import Lexer

main = do
    f <- getLine
    putStrLn $ f ++ ":"
    s <- readFile f
    putStrLn s
    putStrLn ""
    case scanner s of
        Left s -> putStr s
        Right toks -> putStr $ unlines $ map showTokenPos toks