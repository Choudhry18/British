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
  "+"                       { OpTok PlusOp }
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
  eol                       { EOLTok }
  const                    { ConstTok $$}
  int                     { IntTok $$ }
  real                    { Realtok $$}

%left ifz then else
%left "^" "%"
%left "*" "/"
%left "-" "+"
%left "="
%nonassoc sqrt
%nonassoc NEG
%%


S : E eol { ExpS $1}
  | E ms eol {MsS $1}
E : int {IntExp $1} 
  | real {RealExp $1} 
  | const {ConstExp $1} 
  | "(" E ")" {$2} 
  | "-" E {NegExp $2} 
  | sqrt E {SqrtExp $2}
  | E "+" E {BinExp AddOp $1 $3}
  | E "*" E {BinExp MultOp $1 $3}
  | E "/" E {BinExp DivOp $1 $3}
  | E "=" E {BinExp EqOp $1 $3}
  | E "^" E {BinExp ExpOp $1 $3}
  | E "-" E {BinExp SubOp $1 $3}
  | E "%" E {BinExp ModOp $1 $3}
  | ifz E then E else E {IfExp $2 $4 $6}
  | mr {MrExp}
{

data Statement = ExpS Exp | MsS Exp  
data Exp = IntExp Integer | RealExp Double | ConstExp Const | SqrtExp Exp | BinExp Op Exp Exp | IfExp Exp Exp Exp | MrExp 
           |NegExp Exp deriving (Show, Eq)

parseError :: [Token] -> Maybe a
parseError _ = Nothing

expOfStr str = parse =<< scanTokens str

}

