import Parser
import Lexer
import Eval

repl :: IO ()
repl = loop Context{env = [], store = [], classes =[]} 1
    where
        loop context count = do
            input <- getBlock
            if input == ":quit"
                then putStrLn "Goodbye!"
                else do
                    case scanTokens input of
                        Just [] -> loop context (count + 1)
                        Just tokens -> case parse tokens of
                            Just statement -> case evalD context statement of
                                Just (UnitVal, newContext) -> do
                                    loop newContext (count + 1)
                                Just (val, newContext) -> do
                                    putStr "> "
                                    putStrLn $ show val
                                    loop newContext (count + 1)
                                Nothing -> putStrLn $ "Knobhead made an error on " ++ show count
                            Nothing -> putStrLn $ "Bonkers on line " ++ show count
                        Nothing -> putStrLn $ "Loony on line " ++ show count
                    loop context (count + 1)


main :: IO ()
main = repl

getBlock :: IO String
getBlock = aux ""
  where 
    aux :: String -> IO String
    aux prevStr = do
      line <- getLine
      let newLine = concat [prevStr, "\n", line]
      case scanTokens line of
        Just [] -> aux prevStr
        Just tks ->
          if last tks == InnitTok
            then return newLine
            else aux newLine
        Nothing -> return newLine


