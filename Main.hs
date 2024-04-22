import Parser
import Lexer
import Eval(evalS)

repl :: IO ()
repl = do 
    loop
    where
    loop = do
        putStr "> "
        input <- getLine
        if input == ":quit"
            then putStrLn "Goodbye!"
            else do
                case scanTokens input of
                    Just [] -> loop
                    Just tokens -> case parse tokens of
                        Just statement -> case evalS [] statement of
                            Just (_,val) -> putStrLn $ show(val)
                            Nothing -> print "Evaluation Error"
                        Nothing -> putStrLn "Parse Error"
                    Nothing -> putStrLn "Lex Error"
                loop
main :: IO ()
main = repl
