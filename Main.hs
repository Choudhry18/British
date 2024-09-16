import Parser
import Lexer
import Eval

repl :: IO ()
repl = loop Context{env = [], store = [], classes =[]} 1
    where
        loop context count = do
            (c,input) <- getBlock
            do
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

getBlock :: IO (Int, [Token])
getBlock = aux 0 []
  where 
    aux :: Int -> [Token] -> IO (Int, [Token])
    aux lineCount prevStr = do
      line <- getLine
{-       let newLine = concat [prevStr, "\n", line] -}
      case scanTokens line of
        Just [] -> aux (lineCount+1) prevStr  -- Continue if line is empty
        Just tks ->
            return (lineCount + 1, endCheck tks [])  -- Return the accumulated string and line count
        Nothing -> return (lineCount + 1, [])          -- Continue reading input if scanning fails and increment line count

endCheck :: [Token] -> [Token] -> [Token]
endCheck [] out = out
endCheck (x:xs) out = case x of 
    QuitTok -> out ++ [QuitTok]
    others -> endCheck xs (out ++ [x])