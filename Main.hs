import Parser
import Lexer
import Eval(evalS)

repl :: IO ()
repl = do 
    loop []
    where
    loop env = do
        input <- getLine
        if input == ":quit"
            then putStrLn "Goodbye!"
            else do
                case scanTokens input of
                    Just [] -> loop env
                    Just tokens -> case parse tokens of
                        Just statement -> case evalS env statement of  -- Pass env as the first argument
                            Just (newEnv, val) -> do
                                putStr "> "
                                putStrLn $ show val
                                loop newEnv  -- Pass newEnv to the next iteration
                            Nothing -> print "Evaluation Error"
                        Nothing -> putStrLn "Parse Error"
                    Nothing -> putStrLn "Lex Error"
                loop env

main :: IO ()
main = repl
