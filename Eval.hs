module Eval where 
import Parser
import Lexer
import Distribution.Compat.Lens (_1)



data Value = IntVal Integer | RealVal Double 

instance Show Value where 
    show :: Value -> String
    show (IntVal i) = show i
    show (RealVal r) = show r

negateV :: Value -> Value
negateV (IntVal val) = IntVal(negate val)
negateV (RealVal val) = RealVal(negate val)

sqrtV :: Value -> Value
sqrtV (RealVal val) = RealVal $ sqrt val
sqrtV (IntVal val) = RealVal $ sqrt $ fromIntegral val


arithOp :: (Integer -> Integer -> Integer) -> (Double -> Double -> Double) -> Value -> Value -> Value
arithOp f _ (IntVal val1) (IntVal val2) = IntVal (f val1 val2)
arithOp _ g (RealVal val1) (IntVal val2) = RealVal (g val1 (fromIntegral val2))
arithOp _ g (IntVal val1) (RealVal val2) = RealVal (g val2 (fromIntegral val1))
arithOp _ g (RealVal val1) (RealVal val2) = RealVal (g val1 val2)


eval :: Exp -> Value
eval (BinExp AddOp exp1 exp2) = arithOp (+) (+) (eval exp1) (eval exp2)
eval (BinExp SubOp exp1 exp2) = arithOp (-) (-) (eval exp1) (eval exp2) 
eval (BinExp MultOp exp1 exp2) = arithOp (*) (*) (eval exp1) (eval exp2) 
eval (BinExp ExpOp exp1 exp2) = arithOp (^) (**) (eval exp1) (eval exp2)
eval (BinExp ModOp exp1 exp2) = let helper :: Value -> Integer
                                    helper (IntVal val) = val 
                                    helper (RealVal val) = truncate val
                                in IntVal(helper(eval exp1) `mod` helper(eval exp2))
eval (BinExp DivOp exp1 exp2) =
    let dividend = eval exp1
        divisor = eval exp2
        remainder = eval (BinExp ModOp exp1 exp2)
    in case (dividend, divisor, remainder) of
        (IntVal x, IntVal y, IntVal 0) -> IntVal(x `div` y)
        (IntVal x, RealVal y, IntVal 0) -> IntVal(truncate(fromIntegral x/y))
        (RealVal x, IntVal y, IntVal 0) -> IntVal(truncate(x/fromIntegral y))
        (RealVal x, RealVal y, IntVal 0) -> IntVal(truncate(x/y))       
        (RealVal x, RealVal y, _) -> RealVal (x / y)
        (RealVal x, IntVal y, _) -> RealVal (x / fromIntegral y)
        (IntVal x, RealVal y, _) -> RealVal (fromIntegral x / y)
        (IntVal x, IntVal y, _) -> RealVal (fromIntegral x / fromIntegral y)
eval (ConstExp Pi) = RealVal pi
eval (ConstExp Fee) = RealVal (exp 1)
eval (ConstExp Phi) = RealVal ((sqrt 5 + 2)/2)
eval (ConstExp Mole) = RealVal 6.02214076e23
eval (IntExp val) = IntVal val
eval (RealExp val) = RealVal val
eval (IfExp arg1 arg2 arg3) = case eval arg1 of 
    IntVal 0 -> eval arg2
    RealVal 0 -> eval arg2
    _ -> eval arg3
eval MrExp = IntVal 0
eval (NegExp exp) = negateV(eval exp)
eval (SqrtExp exp) = sqrtV(eval exp)



