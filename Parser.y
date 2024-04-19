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
  "%"                        { OpTok ModOp }
  mr                        { MRTok }
  ms                        { MSTok }
  ifz                        { IfzTok}
  then                      { ThenTok }
  else                      { ElseTok }
  "("                       { LeftPTok }
  ")"                       { RightPTok }
  sqrt                     { SqrtTok }
  const                    { ConstTok $$}
  int                     { IntTok $$ }
  real                    { Realtok $$}
  "/\\"                   {OpTok AndOp}
  "\\/"                   {OpTok OrOp}
  ">"                     {OpTok GOp}
  "<"                     {OpTok LOp}
  leq                     {OpTok LeqOp}
  geq                     {OpTok GeqOp}



%nonassoc NEG
%right "\\/"
%right "/\\"
%nonassoc "=" "<" ">" leq geq 
%left "-" "+"
%nonassoc sqrt
%left "%" "*" "/"
%right "^"
%left else
%%


S : E { ExpS $1}
  | E ms {MsS $1}
E : int {IntExp $1} 
  | real {RealExp $1} 
  | const {ConstExp $1} 
  | "(" E ")" {$2} 
  | "-" E %prec NEG{NegExp $2} 
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
  | mr {MrExp}
{

data Statement = ExpS Exp | MsS Exp  deriving Show
data Exp = IntExp Integer | RealExp Double | ConstExp Const | SqrtExp Exp | BinExp Op Exp Exp | IfExp Exp Exp Exp | MrExp 
           |NegExp Exp  deriving (Show, Eq)

parseError :: [Token] -> Maybe a
parseError _ = Nothing

expOfStr str = parse =<< scanTokens str

}

