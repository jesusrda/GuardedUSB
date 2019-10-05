import Lexer

main = do
    s <- getContents
    case scanner s of
        Left s -> print s
        Right toks -> loop toks
            where loop [] = return ()
                  loop (x:xs) = do printTokenPos x
                                   loop xs