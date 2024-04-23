import Parser
import Lexer
import Eval (evalS)

repl :: IO ()
repl = loop [] 1
    where
        loop env count = do
            input <- getLine
            if input == ":quit"
                then putStrLn "Goodbye!"
                else do
                    case scanTokens input of
                        Just [] -> loop env (count + 1)
                        Just tokens -> case parse tokens of
                            Just statement@(ExpS _) -> case evalS env statement of
                                Just (newEnv, val) -> do
                                    putStr "> "
                                    putStrLn $ show val
                                    loop newEnv (count + 1)
                                Nothing -> putStrLn $ "Evaluation Error on line " ++ show count
                            Just declaration@(DecS _ _) -> case evalS env declaration of
                                Just (newEnv, _) -> loop newEnv (count+1)
                                Nothing -> putStrLn $ "Evaluation Error on line " ++ show count
                            Nothing -> putStrLn $ "Parse Error on line " ++ show count
                        Nothing -> putStrLn $ "Lex Error on line " ++ show count
                    loop env (count + 1)

main :: IO ()
main = repl