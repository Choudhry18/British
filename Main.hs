import Parser
import Lexer
import Eval

repl :: IO ()
repl = loop Context{env = [], store = [], classes =[]} 1
    where
        loop context count = do
            (c,input) <- getBlock
            if input == ":quit"
                then putStrLn "Goodbye!"
                else do
                    case scanTokens input of
                        Just [] -> loop context (count + c)
                        Just tokens -> case parse tokens of
                            Just statement -> case evalD context statement of
                                Just (UnitVal, newContext) -> do
                                    loop newContext (count + c)
                                Just (val, newContext) -> do
                                    putStr "> "
                                    putStrLn $ show val
                                    loop newContext (count + c)
                                Nothing -> putStrLn $ "Knobhead made an error on " ++ show (count+c)
                            Nothing -> putStrLn $ "Bonkers on line " ++ show (count+c)
                        Nothing -> putStrLn $ "Loony on line " ++ show (count+c)
                    loop context (count + c)


main :: IO ()
main = repl

getBlock :: IO (Int, String)
getBlock = aux 0 ""
  where 
    aux :: Int -> String -> IO (Int, String)
    aux lineCount prevStr = do
      line <- getLine
      let newLine = concat [prevStr, "\n", line]
      case scanTokens line of
        Just [] -> aux (lineCount+1) prevStr  -- Continue if line is empty
        Just tks ->
          if last tks == InnitTok  -- Check if the last token is InnitTok
            then return (lineCount + 1, newLine)  -- Return the accumulated string and line count
            else aux (lineCount + 1) newLine     -- Continue reading input and increment line count
        Nothing -> aux lineCount newLine         -- Continue reading input if scanning fails and increment line count


