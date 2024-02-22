{
module Lexer where
import Debug.Trace
}
%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :- 
  =                        { const EqTok }
  \+                       { const AddTok }
  \-                       { const SubTok }
  \*                       { const MultTok }
  \^                       { const ExpTok }
  \/                       { const DivTok}
  MR                       { const MRTok }
  MS                       { const MSTok }
  ifz                      { const IfzTok}
  then                     { const ThenTok }
  else                     { const ElseTok }
  \(                       { const LeftPTok }
  \)                       { const RightPTok }
  \%                       { const ModTok }
  sqrt                     { const SqrtTok }
  "\\n"                    { const EOLTok }
  pie                      { const PiTok }
  fee                      { const FeeTok }
  phi                      { const PhiTok }
  mole                     { const MoleTok }
  $white                   ;
  [a-z][$alpha $digit]*    { VarTok }
  \-? $digit+              { IntTok . read }
  \-? $digit+ \. $digit+   { Realtok . read }
{
data Token = EqTok | AddTok | SubTok | MultTok | DivTok | ExpTok | ModTok | IfzTok | ThenTok | ElseTok | EOLTok | MRTok | MSTok | LeftPTok | RightPTok | SqrtTok | PiTok | FeeTok | PhiTok | MoleTok | VarTok String | IntTok Integer | Realtok Double deriving (Show, Eq) 
scanTokens :: String -> Maybe [Token]
scanTokens str = go ('\n',[],str)
  where go inp@(_,_bs,str) =
          case alexScan inp 0 of
                AlexEOF -> Just []
                AlexError err -> traceShow err $ Nothing
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> fmap ((act (take len str)):) (go inp')
}