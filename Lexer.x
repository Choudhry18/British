{
module Lexer where
import Debug.Trace
}
%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :- 
  =                        { const (OpTok EqOp) }
  \+                       { const (OpTok AddOp) }
  \-                       { const (OpTok SubOp) }
  \*                       { const (OpTok MultOp) }
  \^                       { const (OpTok ExpOp)}
  \/                       { const (OpTok DivOp)}
  \%                       { const (OpTok DivOp) }
  MR                       { const MRTok }
  MS                       { const MSTok }
  ifz                      { const IfzTok}
  then                     { const ThenTok }
  else                     { const ElseTok }
  \(                       { const LeftPTok }
  \)                       { const RightPTok }
  sqrt                     { const SqrtTok }
  "\\n"                    { const EOLTok }
  pie                      { const (ConstTok Pi)}
  fee                      { const (ConstTok Fee)}
  phi                      { const (ConstTok Phi)}
  mole                     { const (ConstTok Mole)}
  $white                   ;
  [a-z][$alpha $digit]*    { VarTok }
  \-? $digit+              { IntTok . read }
  \-? $digit+ \. $digit+   { Realtok . read }
{
data Token = OpTok Op| ConstTok Const | IfzTok | ThenTok | ElseTok | EOLTok | MRTok | MSTok | LeftPTok | RightPTok | SqrtTok 
             | VarTok String | IntTok Integer | Realtok Double deriving (Show, Eq) 

data Op = EqOp | AddOp | SubOp | MultOp | DivOp | ExpOp | ModOp deriving (Show, Eq) 

data Const = Pi | Fee | Phi | Mole deriving (Show, Eq) 

scanTokens :: String -> Maybe [Token]
scanTokens str = go ('\n',[],str)
  where go inp@(_,_bs,str) =
          case alexScan inp 0 of
                AlexEOF -> Just []
                AlexError err -> traceShow err $ Nothing
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> fmap ((act (take len str)):) (go inp')
}