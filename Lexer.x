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
  [A-Z_]+                { VarTok }
  $digit+              { IntTok . read }
  $digit+ \. $digit+   { Realtok . read }
  supposing            {const SupposingTok}
  hence                {const HenceTok}
  otherwise            {const OtherwiseTok}
  hearye               {const HearyeTok}
  oi                   {const OiTok}  
  is                   {const IsTok}        
  for                  {const ForTok}
  \/\\                 {const (OpTok AndOp)}
  \\\/                 {const (OpTok OrOp)}
  ">"                  {const (OpTok GOp)}
  "<"                  {const (OpTok LOp)}
  leq                  {const (OpTok LeqOp)}
  geq                  {const (OpTok GeqOp)}
  innit                {const InnitTok}
  ace                  {const TrueTok}
  rank                 {const FalseTok}
{
data Token = OpTok Op| ConstTok Const | IfzTok | ThenTok | ElseTok | EOLTok | MRTok | MSTok | LeftPTok | RightPTok | SqrtTok 
             | VarTok String | IntTok Integer | Realtok Double | SupposingTok | HenceTok | OtherwiseTok | HearyeTok | OiTok
             | IsTok | ForTok | InnitTok | TrueTok | FalseTok deriving (Show, Eq) 

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