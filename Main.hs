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
                    Just (ExpS expr) -> case eval expr of
                        Nothing -> print "error"
                        val -> print $ show(val)
                loop
main :: IO ()
main = repl
