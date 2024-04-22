module Eval where 
import Parser
import Lexer
import Distribution.Compat.Lens (_1)



data Value = IntVal Integer | RealVal Double | StringVal String | BoolVal Bool 

instance Show Value where 
    show :: Value -> String
    show (IntVal i) = show i
    show (RealVal r) = show r
    show (BoolVal b) = show b
    show (StringVal s) = s

negateV :: Maybe Value -> Maybe Value
negateV (Just(IntVal val)) = Just $ IntVal(negate val)
negateV (Just(RealVal val)) = Just $ RealVal(negate val)
negateV (Just(BoolVal val)) = Just $ BoolVal(not val)
negateV _ = Nothing

sqrtV :: Maybe Value -> Maybe Value
sqrtV (Just(RealVal val)) = Just $ RealVal $ sqrt val
sqrtV (Just(IntVal val)) = Just $ RealVal $ sqrt $ fromIntegral val
sqrtV _ = Nothing

arithOp :: (Integer -> Integer -> Integer) -> (Double -> Double -> Double) -> Maybe Value -> Maybe Value -> Maybe Value
arithOp f _ (Just(IntVal val1)) (Just(IntVal val2)) = Just $ IntVal (f val1 val2)
arithOp _ g (Just(RealVal val1)) (Just(IntVal val2)) = Just $ RealVal (g val1 (fromIntegral val2))
arithOp _ g (Just(IntVal val1)) (Just(RealVal val2)) = Just $ RealVal (g val2 (fromIntegral val1))
arithOp _ g (Just(RealVal val1)) (Just(RealVal val2)) = Just $ RealVal (g val1 val2)
arithOp _ _ _ _ = Nothing


modHelper :: Maybe Value -> Maybe Value -> Maybe Integer
modHelper (Just val1) (Just val2) = let helper :: Maybe Value -> Maybe Integer
                                        helper (Just(IntVal val)) = Just val 
                                        helper (Just(RealVal val)) = Just $ truncate val
                                        helper _ = Nothing
                      in case (helper(Just val1), helper(Just val2)) of
                        (Just int1, Just int2) -> Just (int1 `mod` int2)
                        (_ , _ ) -> Nothing

eval :: Exp -> Maybe Value
eval (BinExp AddOp exp1 exp2) = arithOp (+) (+) (eval exp1) (eval exp2)
eval (BinExp SubOp exp1 exp2) = arithOp (-) (-) (eval exp1) (eval exp2) 
eval (BinExp MultOp exp1 exp2) = arithOp (*) (*) (eval exp1) (eval exp2) 
eval (BinExp ExpOp exp1 exp2) = arithOp (^) (**) (eval exp1) (eval exp2)
eval (BinExp ModOp exp1 exp2) = let helper :: Maybe Value -> Maybe Integer
                                    helper (Just(IntVal val)) = Just val 
                                    helper (Just(RealVal val)) = Just $ truncate val
                                    helper _ = Nothing
                                in case (helper $ eval exp1, helper $ eval exp2) of
                                   (Just val1, Just val2) -> Just $ IntVal val1
                                   (_ , _) -> Nothing
eval (BinExp DivOp exp1 exp2) =
    let dividend = eval exp1
        divisor = eval exp2
        remainder = modHelper dividend divisor
    in case (dividend, divisor, remainder) of
        (Just (IntVal x), Just (IntVal y), Just 0) -> Just $ IntVal(x `div` y)
        (Just (IntVal x), Just(RealVal y), Just 0) -> Just $ IntVal(truncate(fromIntegral x/y))
        (Just (RealVal x), Just(IntVal y), Just 0) -> Just $ IntVal(truncate(x/fromIntegral y))
        (Just (RealVal x), Just (RealVal y), Just 0) -> Just $ IntVal(truncate(x/y))       
        (Just (RealVal x), Just (RealVal y), _) -> Just $ RealVal (x / y)
        (Just (RealVal x), Just (IntVal y), _) -> Just $ RealVal (x / fromIntegral y)
        (Just (IntVal x), Just (RealVal y), _) -> Just $ RealVal (fromIntegral x / y)
        (Just (IntVal x), Just (IntVal y), _) -> Just $ RealVal (fromIntegral x / fromIntegral y)
        (_ , _ , _) -> Nothing
eval (BinExp AndOp exp1 exp2) = case eval exp1 of
    Nothing -> Nothing
    Just (BoolVal False) -> Just $ BoolVal False
    Just (BoolVal True)  -> case eval exp2 of 
        Nothing -> Nothing
        Just (BoolVal False) -> Just (BoolVal False)
        Just (BoolVal True)  -> Just (BoolVal True)
eval (ConstExp Pi) = Just $ RealVal pi
eval (ConstExp Fee) = Just $ RealVal (exp 1)
eval (ConstExp Phi) = Just $ RealVal ((sqrt 5 + 2)/2)
eval (ConstExp Mole) = Just $ RealVal 6.02214076e23
eval (IntExp val) = Just $ IntVal val
eval (RealExp val) = Just $ RealVal val
eval (BoolExp val) = Just $ BoolVal val
eval (StringExp val) = Just $ StringVal val
eval (IfExp arg1 arg2 arg3) = case eval arg1 of 
    Just (IntVal 0) -> eval arg2
    Just (RealVal 0) -> eval arg2
    _ -> eval arg3
eval (NegExp exp) = negateV(eval exp)
eval (SqrtExp exp) = sqrtV(eval exp)
eval _ = Nothing



