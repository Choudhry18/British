import Parser
import Eval(eval)

repl :: IO ()
repl = do 
    putStrLn "Enter line: "
    loop
    where
    loop = do
        putStr "> "
        input <- getLine
        if input == ":quit"
            then putStrLn "Goodbye!"
            else do
                case expOfStr input of
                    Nothing -> putStrLn "Error"
                    Just (ExpS expr) -> putStrLn $ "Parsed successfully: " ++  show (eval expr)
                loop
main :: IO ()
main = repl
