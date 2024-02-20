{
module Lexer where
import Debug.Trace
}
%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :- 
  let                      { const LetTok }
  in                       { const InTok }
  =                        { const EqTok }
  $white                   ;
  [a-z][$alpha $digit]*    { VarTok }
  \-? $digit+              { IntTok . read }

{
data Token = LetTok | EqTok | InTok | VarTok String | IntTok Integer deriving (Show, Eq)

scanTokens :: String -> Maybe [Token]
scanTokens str = go ('\n',[],str)
  where go inp@(_,_bs,str) =
          case alexScan inp 0 of
                AlexEOF -> Just []
                AlexError err -> traceShow err $ Nothing
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> fmap ((act (take len str)):) (go inp')
}