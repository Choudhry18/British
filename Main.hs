import Parser
import Eval(eval)

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
                case expOfStr input of
                    Nothing -> putStrLn "Error"
                    Just (ExpS expr) -> print $ show(eval expr)
                loop
main :: IO ()
main = repl
