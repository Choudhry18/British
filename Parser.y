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
  "\\"                      { SndTok } 
  ","                       { ComTok}
  leq                       { OpTok LeqOp}
  geq                       { OpTok GeqOp}
  ifz                       { IfzTok}
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
  "#"                         { UnitTok }
  colonize                  { ColonizeTok }
  cheers                    { CheersTok }
  mate                      { MateTok }
  bloke                     { BlokeTok }
  ";"                       { SemiTok}
  "=>"                      { RocketTok}
  const                     { ConstTok $$}
  int                       { IntTok $$ }
  real                      { Realtok $$}
  ace                       { BoolTok True}
  rank                      { BoolTok False}
  var                       { VarTok $$}
  string                    { StringTok $$}
  display                   { DisplayTok }
  "=="                      { MutateTok }
  whilst                    { WhileTok}
  doeth                     { DoTok}
  "|"                       { DeRefTok}
  "~"                       { SeqTok }
  noble                     { NobleTok }
  serfs                     { SerfTok }
  obeys                     { ObeyTok }   
  decree                    { DecreeTok }
  a                         { ATok }
  "."                       { DotTok }
  "{"                       { LCurlyTok }
  "}"                       { RCurlyTok }
  fname                     { FNameTok $$ }
  and                       { AndTok }
  ":quit"                     { QuitTok }

%nonassoc doeth otherwise for "=>"
%right "~"
%left  "==" 
%nonassoc display
%left otherwise for
%right "\\/" "/\\"
%nonassoc "=" "<" ">" leq geq 
%left "-" "+"
%left "%"
%left "*" "/"
%right "^"
%nonassoc NEG sqrt
%nonassoc "."
%left "(" ")"
%%


D : E innit { ExpS $1}
  | hearye var is E innit {DecS $2 $4}
  | colonize var is E innit {RecS $2 $4}
  | noble var "{"M"}" innit {CDec $2 $4}
  | noble var obeys var "{"M"}" innit {InheritDec $2 $4 $6}
  | ":quit" { QuitS }
M : serfs FList MList {$2 ++ $3}
  | MList {$1}
FList : fname {[FieldD $1 ]}
  | fname"," FList {(FieldD $1) : $3 }
MList : {- empty -} {[]} 
  | decree fname is E MList {(MethodD $2 $4) : $5}
EList : {- empty -} { [] }
  | E { [$1] }
  | E and EList { $1 : $3 }
E : int {IntExp $1} 
  | real {RealExp $1} 
  | const {ConstExp $1}
  | ace  {BoolExp True} 
  | rank {BoolExp False}
  | string {StringExp $1}
  | var    {VarExp $1}
  | "(" E ")" {$2} 
  | "[" E "]" {NegBExp $2}
  | mate"("E")" {MateExp $3}
  | bloke"("E")" {BlokeExp $3}
  | "#"            {UnitExp}
  | cheers var "=>" E {FuncDExp $2 $4}
  | E"("E")"     {FuncAExp $1 $3}
  | "/"E"," E"\\" {PairExp $2 $4}
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
  | E "<" E {BinExp LOp $1 $3}
  | E leq E {BinExp LeqOp $1 $3}
  | E geq E {BinExp GeqOp $1 $3}
  | ifz E hence E otherwise E {IfExp $2 $4 $6}
  | supposing E hence E otherwise E {HenceExp $2 $4 $6}
  | oi var is E for E {LDeclExp $2 $4 $6}
  | display E {DisplayExp $2}
  | "|"E"|"   {DeRefExp $2}
  | E "==" E    {MutExp $1 $3}
  | E "~" E     {SeqExp $1 $3}
  | whilst E doeth E {WhileExp $2 $4}
  | a var "{"EList"}" {NewExp $2 $4 }
  | E"."fname           {LookupExp $1 $3}
{ 

type Var = String
data Statement = ExpS Exp | DecS Var Exp | RecS Var Exp | CDec String [CElemD] | QuitS | InheritDec String String [CElemD] deriving Show
data Exp = IntExp Integer | RealExp Double | ConstExp Const | BoolExp Bool | VarExp Var | StringExp String | SqrtExp Exp 
           | BinExp Op Exp Exp | IfExp Exp Exp Exp | HenceExp Exp Exp Exp |NegExp Exp |NegBExp Exp |LDeclExp String Exp Exp | MateExp Exp 
           | BlokeExp Exp | UnitExp | FuncDExp String Exp | FuncAExp Exp Exp | PairExp Exp Exp | DisplayExp Exp | DeRefExp Exp 
           | MutExp Exp Exp | WhileExp Exp Exp | SeqExp Exp Exp | NewExp String [Exp] | LookupExp Exp String deriving (Show, Eq)
data CElemD = FieldD Var| MethodD Var Exp deriving Show
parseError :: [Token] -> Maybe a
parseError _ = Nothing

expOfStr str = parse =<< scanTokens str

}

