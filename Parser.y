{
module Parser where
import Lexer
import Data.Maybe
}

%monad { Maybe } 
%name parse
%tokentype {Token}
%error { parseError}

%token

  "="                       { OpTok EqOp }
  "+"                       { OpTok AddOp }
  "-"                       { OpTok SubOp }
  "*"                       { OpTok MultOp }
  "^"                       { OpTok ExpOp }
  "/"                       { OpTok DivOp}
  "%"                       { OpTok ModOp }
  "/\\"                     { OpTok AndOp}
  "\\/"                     { OpTok OrOp}
  ">"                       { OpTok GOp}
  "<"                       { OpTok LOp}
  leq                       { OpTok LeqOp}
  geq                       { OpTok GeqOp}
  ifz                       { IfzTok}
  then                      { ThenTok }
  else                      { ElseTok }
  "("                       { LeftPTok }
  ")"                       { RightPTok }
  "["                       { LeftBTok }
  "]"                       { RightBTok }
  sqrt                      { SqrtTok }
  supposing                 { SupposingTok}
  hence                     { HenceTok}
  otherwise                 { OtherwiseTok}
  hearye                    { HearyeTok}
  oi                        { OiTok}
  is                        { IsTok}
  for                       { ForTok}
  innit                     { InnitTok}
  const                     { ConstTok $$}
  int                       { IntTok $$ }
  real                      { Realtok $$}
  ace                       { BoolTok True}
  rank                      { BoolTok False}
  var                       { VarTok $$}
  string                    { StringTok $$}


%nonassoc for
%nonassoc NEG
%right "\\/"
%right "/\\"
%nonassoc "=" "<" ">" leq geq 
%left "-" "+"
%nonassoc sqrt
%left "%" "*" "/"
%right "^"
%left else otherwise
%%


S : E innit { ExpS $1}
  | hearye var is E innit {DecS $2 $4}
E : int {IntExp $1} 
  | real {RealExp $1} 
  | const {ConstExp $1}
  | ace  {BoolExp True} 
  | rank {BoolExp False}
  | string {StringExp $1}
  | var    {VarExp $1}
  | "(" E ")" {$2} 
  | "[" E "]" {NegExp $2}
  | "-" E {NegExp $2} 
  | sqrt E {SqrtExp $2}
  | E "+" E {BinExp AddOp $1 $3}
  | E "*" E {BinExp MultOp $1 $3}
  | E "/" E {BinExp DivOp $1 $3}
  | E "=" E {BinExp EqOp $1 $3}
  | E "^" E {BinExp ExpOp $1 $3}
  | E "-" E {BinExp SubOp $1 $3}
  | E "%" E {BinExp ModOp $1 $3}
  | E "/\\" E {BinExp AndOp $1 $3}
  | E "\\/" E {BinExp OrOp $1 $3}
  | E ">" E {BinExp GOp $1 $3}
  | E "<" E {BinExp GOp $1 $3}
  | E leq E {BinExp LeqOp $1 $3}
  | E geq E {BinExp GeqOp $1 $3}
  | ifz E then E else E {IfExp $2 $4 $6}
  | supposing E hence E otherwise E {HenceExp $2 $4 $6}
  | oi var is E for E {LDeclExp $2 $4 $6}
{

type Var = String
data Statement = ExpS Exp | DecS Var Exp deriving Show
data Exp = IntExp Integer | RealExp Double | ConstExp Const | BoolExp Bool | VarExp Var | StringExp String | SqrtExp Exp | BinExp Op Exp Exp 
           | IfExp Exp Exp Exp | HenceExp Exp Exp Exp |NegExp Exp |LDeclExp String Exp Exp deriving (Show, Eq)

parseError :: [Token] -> Maybe a
parseError _ = Nothing

expOfStr str = parse =<< scanTokens str

}

