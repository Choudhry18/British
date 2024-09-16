import Parser
import Lexer
import Eval

repl :: IO ()
repl = loop Context {env = [], store = [], classes = []} 1
  where
    loop context count = do
      (c, tokens) <- getBlock
      if null tokens then
        loop context (count + c)  -- No tokens, continue REPL loop
      else
        case parse tokens of
          Just QuitS -> putStr "> Thank you for using the interpreter"
          Just statement ->
            case evalD context statement of
              Just (UnitVal, newContext) -> loop newContext (count + c)
              Just (val, newContext) -> do
                putStr "> "
                print val
                loop newContext (count + c)
              Nothing -> do
                putStrLn $ "Evaluation error on line " ++ show (count + c)
                loop context (count + c)
          Nothing -> do
            putStrLn $ "Parsing error on line " ++ show (count + c)
            loop context (count + c)

main :: IO ()
main = repl

getBlock :: IO (Int, [Token])
getBlock = aux 0 []
  where
    aux :: Int -> [Token] -> IO (Int, [Token])
    aux lineCount prevTokens = do
      line <- getLine
      case scanTokens line of
        Just [] -> aux (lineCount + 1) prevTokens  -- Continue if line is empty
        Just tks -> return (lineCount + 1, endCheck tks prevTokens)  -- Check tokens
        Nothing -> do
          putStrLn $ "Scanning failed on line " ++ show (lineCount + 1)
          return (lineCount, prevTokens)

endCheck :: [Token] -> [Token] -> [Token]
endCheck [] out = out
endCheck (x:xs) out = case x of
  QuitTok -> out ++ [QuitTok]  -- Stop if QuitTok is encountered
  _       -> endCheck xs (out ++ [x])
