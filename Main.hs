import Parser
import Lexer
import Eval

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
                            Just statement -> case evalS env statement of
                                Just (newEnv, UnitVal) -> do
                                    loop newEnv (count + 1)
                                Just (newEnv, val) -> do
                                    putStr "> "
                                    putStrLn $ show val
                                    loop newEnv (count + 1)
                                Nothing -> putStrLn $ "Knobhead made an error on " ++ show count
                            Nothing -> putStrLn $ "Bonkers on line " ++ show count
                        Nothing -> putStrLn $ "Loony on line " ++ show count
                    loop env (count + 1)

main :: IO ()
main = repl