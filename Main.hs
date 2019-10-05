import Lexer

main = do
    f <- getLine
    putStrLn f
    s <- readFile f
    putStrLn s
    case scanner s of
        Left s -> putStr s
        Right toks -> loop toks
            where loop [] = return ()
                  loop (x:xs) = do printTokenPos x
                                   loop xs