import Lexer

main = do
    f <- getLine
    print $ f++"\n"
    s <- readFile f
    print $ s++"\n"
    case scanner s of
        Left s -> print s
        Right toks -> loop toks
            where loop [] = return ()
                  loop (x:xs) = do printTokenPos x
                                   loop xs