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
  \%                       { const (OpTok ModOp) }
  \/\\                     { const (OpTok AndOp)}
  \\\/                     { const (OpTok OrOp)}
  ">"                      { const (OpTok GOp)}
  "<"                      { const (OpTok LOp)}
  leq                      { const (OpTok LeqOp)}
  geq                      { const (OpTok GeqOp)}
  ifz                      { const IfzTok}
  then                     { const ThenTok }
  else                     { const ElseTok }
  \(                       { const LeftPTok }
  \)                       { const RightPTok }
  "["                      { const LeftBTok}
  "]"                      { const RightBTok}
  sqrt                     { const SqrtTok }
  "\\n"                    { const EOLTok }
  supposing                { const SupposingTok}
  hence                    { const HenceTok}
  otherwise                { const OtherwiseTok}
  hearye                   { const HearyeTok}
  oi                       { const OiTok}  
  is                       { const IsTok}        
  for                      { const ForTok}
  innit                    { const InnitTok}
  pie                      { const (ConstTok Pi)}
  fee                      { const (ConstTok Fee)}
  phi                      { const (ConstTok Phi)}
  mole                     { const (ConstTok Mole)}
  \-\-[.]*                 ;
  $white                   ;
  ``[^``]*``               { StringTok}
  [A-Z_]+                  { VarTok }
  $digit+                  { IntTok . read }
  $digit+ \. $digit+       { Realtok . read }
  ace                      {const (BoolTok True)}
  rank                     {const (BoolTok False)}
{
data Token = OpTok Op| ConstTok Const | IfzTok | ThenTok | ElseTok | EOLTok| LeftPTok | RightPTok | LeftBTok | RightBTok 
             | SqrtTok | VarTok String | IntTok Integer | Realtok Double | SupposingTok | HenceTok | OtherwiseTok | HearyeTok | OiTok
             | IsTok | ForTok | InnitTok | BoolTok Bool | StringTok String deriving (Show, Eq) 

data Op = EqOp | AddOp | SubOp | MultOp | DivOp | ExpOp | ModOp| GOp |LOp| GeqOp | LeqOp |AndOp | OrOp deriving (Show, Eq) 

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