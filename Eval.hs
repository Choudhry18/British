module Eval where
import Parser
import Lexer
import Distribution.Compat.Lens (_1)
import GHC.Real (underflowError)
import Data.Text.Array (new)
import Data.Maybe

type Env = [(Var, Value)]
lookupEnv :: Var -> Env -> Maybe Value
lookupEnv = lookup

updateEnv :: Var -> Value -> Env -> Env
updateEnv var val env = (var,val) : filter (\(x,_) -> x /= var) env
data Value = IntVal Integer | RealVal Double | StringVal String | BoolVal Bool | PairVal Value Value | UnitVal
             | FuncVal String Exp Env deriving Eq

instance Show Value where
    show :: Value -> String
    show (IntVal i) = show i
    show (RealVal r) = show r
    show (BoolVal True) = "ace"
    show (BoolVal False) = "rank"
    show (StringVal s) = s
    show (PairVal fst snd) = "/" ++ show fst ++ ", " ++ show snd ++ "\\"
    show UnitVal = "#"
    show (FuncVal s _ _) = s


negateV :: Maybe Value -> Maybe Value
negateV (Just(IntVal val)) = Just $ IntVal (negate val)
negateV (Just(RealVal val)) = Just $ RealVal (negate val)
negateV (Just(BoolVal val)) = Just $ BoolVal (not val)
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

compOp :: (Integer -> Integer -> Bool) -> (Double -> Double -> Bool) -> Maybe Value -> Maybe Value -> Maybe Value
compOp f g (Just(IntVal val1)) (Just(IntVal val2)) = Just $ BoolVal (f val1 val2)
compOp _ g (Just(RealVal val1)) (Just(IntVal val2)) = Just $ BoolVal (g val1 (fromIntegral val2))
compOp _ g (Just(IntVal val1)) (Just(RealVal val2)) = Just $ BoolVal (g (fromIntegral val1) val2)
compOp _ g (Just(RealVal val1)) (Just(RealVal val2)) = Just $ BoolVal (g val1 val2)
compOp _ _ _ _ = Nothing

modHelper :: Maybe Value -> Maybe Value -> Maybe Integer
modHelper val1 val2 = let helper :: Maybe Value -> Maybe Integer
                          helper (Just(IntVal val)) = Just val
                          helper (Just(RealVal val)) = Just $ truncate val
                          helper _ = Nothing
                      in case (helper val1, helper val2) of
                        (Just int1, Just int2) -> Just (int1 `mod` int2)
                        (_ , _ ) -> Nothing

appMaybe:: (a -> Maybe b) -> Maybe a -> Maybe b --built in as fMap
appMaybe f a = do av <- a
                  f av

appMaybe2 :: (a -> b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
appMaybe2 f a b = do av <- a
                     bv <- b
                     f av bv

evalS :: Env -> Statement -> Maybe (Env,Value)
evalS env (ExpS exp) = case eval env exp of
    Just val -> Just (env, val)
    Nothing -> Nothing
evalS env (DecS var exp) = case eval env exp of
    Nothing -> Nothing
    Just val -> Just (updateEnv var val env, UnitVal)
evalS env (RecS var exp) =
    let newEnv = updateEnv var newVal env
        newVal = fromMaybe UnitVal (eval newEnv exp)
    in Just (newEnv, newVal)
evalS _ _ = Nothing

eval :: Env -> Exp -> Maybe Value
--Arithmetic Expression
eval env (BinExp AddOp exp1 exp2) = arithOp (+) (+) (eval env exp1) (eval env exp2)
eval env (BinExp SubOp exp1 exp2) = arithOp (-) (-) (eval env exp1) (eval env exp2)
eval env (BinExp MultOp exp1 exp2) = arithOp (*) (*) (eval env exp1) (eval env exp2)
eval env (BinExp ExpOp exp1 exp2) = arithOp (^) (**) (eval env exp1) (eval env exp2)
eval env (BinExp ModOp exp1 exp2) = case modHelper (eval env exp1) (eval env exp2) of
    Just val -> Just $ IntVal val
    Nothing -> Nothing

eval env (BinExp DivOp exp1 exp2) =
    let dividend = eval env exp1
        divisor = eval env exp2
        remainder = modHelper dividend divisor
    in case (dividend, divisor, remainder) of
        (Just (IntVal x), Just (IntVal y), Just 0) -> Just $ IntVal (x `div` y)
        (Just (IntVal x), Just(RealVal y), Just 0) -> Just $ IntVal (truncate (fromIntegral x/y))
        (Just (RealVal x), Just(IntVal y), Just 0) -> Just $ IntVal (truncate (x/fromIntegral y))
        (Just (RealVal x), Just (RealVal y), Just 0) -> Just $ IntVal (truncate (x/y))
        (Just (RealVal x), Just (RealVal y), _) -> Just $ RealVal (x / y)
        (Just (RealVal x), Just (IntVal y), _) -> Just $ RealVal (x / fromIntegral y)
        (Just (IntVal x), Just (RealVal y), _) -> Just $ RealVal (fromIntegral x / y)
        (Just (IntVal x), Just (IntVal y), _) -> Just $ RealVal (fromIntegral x / fromIntegral y)
        (_ , _ , _) -> Nothing

--Comparison Expressions
eval env (BinExp GOp exp1 exp2) = compOp (>) (>) (eval env exp1) (eval env exp2)
eval env (BinExp LOp exp1 exp2) = compOp (<) (<) (eval env exp1) (eval env exp2)
eval env (BinExp GeqOp exp1 exp2) = compOp (>=) (>=) (eval env exp1) (eval env exp2)
eval env (BinExp LeqOp exp1 exp2) = compOp (<=) (<=) (eval env exp1) (eval env exp2)
eval env (BinExp EqOp exp1 exp2) = case (eval env exp1, eval env exp2) of
    (Just(BoolVal True), Just(BoolVal True)) -> Just $ BoolVal True
    (Just(BoolVal False), Just(BoolVal False)) -> Just $ BoolVal True
    (Just(BoolVal False), Just(BoolVal True)) -> Just $ BoolVal False
    (Just(BoolVal True), Just(BoolVal False)) -> Just $ BoolVal False
    (Just(BoolVal _), Just(IntVal _)) -> Just $ BoolVal False
    (Just(BoolVal _), Just(RealVal _)) -> Just $ BoolVal False
    (Just(StringVal lstring),Just(StringVal rstring)) -> Just $ BoolVal (lstring == rstring)
    (Just UnitVal, Just UnitVal) -> Just $ BoolVal True
    (Just (PairVal valL1 valL2), Just (PairVal valR1 valR2)) -> Just $ BoolVal $ (valL1 == valR1) && (valL2 == valR2)
    (Just (FuncVal {}), _) -> Just $ BoolVal False
    (_, Just (FuncVal {})) -> Just $ BoolVal False
    (x, y) -> compOp (==) (==) x y

--Logical Expressions
eval env (BinExp AndOp exp1 exp2) = case eval env exp1 of
    Just (BoolVal False) -> Just $ BoolVal False
    Just (BoolVal True)  -> case eval env exp2 of
        Just (BoolVal False) -> Just (BoolVal False)
        Just (BoolVal True)  -> Just (BoolVal True)
        _ -> Nothing
    _ -> Nothing
eval env (BinExp OrOp exp1 exp2) = case eval env exp1 of
    Just (BoolVal True) -> Just $ BoolVal True
    Just (BoolVal False) -> case eval env exp2 of
        Just (BoolVal True) -> Just $ BoolVal True
        Just (BoolVal False) -> Just $ BoolVal False
        _ -> Nothing
    _ -> Nothing

--Constants
eval env (ConstExp Pi) = Just $ RealVal pi
eval env (ConstExp Fee) = Just $ RealVal (exp 1)
eval env (ConstExp Phi) = Just $ RealVal ((sqrt 5 + 2)/2)
eval env (ConstExp Mole) = Just $ RealVal 6.02214076e23

--Values
eval env (IntExp val) = Just $ IntVal val
eval env (RealExp val) = Just $ RealVal val
eval env (BoolExp val) = Just $ BoolVal val
eval env (StringExp val) = Just $ StringVal val
eval env (VarExp val) = lookupEnv val env

--Conditional
eval env (IfExp arg1 arg2 arg3) = case eval env arg1 of
    Just (IntVal 0) -> eval env arg2
    Just (RealVal 0) -> eval env arg2
    Nothing -> Nothing
    _ -> eval env arg3
eval env (HenceExp arg1 arg2 arg3) = case eval env arg1 of
    Just (BoolVal True) -> eval env arg2
    Just (BoolVal False) -> eval env arg3
    _ -> Nothing

--Pairs
eval env (PairExp exp1 exp2) = case (eval env exp1, eval env exp2) of
    (Just val1, Just val2) -> Just $ PairVal val1 val2
    _ -> Nothing
eval env UnitExp = Just UnitVal
eval env (MateExp exp) = case eval env exp of
    Just (PairVal ft _) -> Just ft
    _ -> Nothing
eval env (BlokeExp exp) = case eval env exp of
    Just (PairVal _ sd) -> Just sd
    _ -> Nothing

--Functions
eval env (FuncDExp parameter body) = Just $ FuncVal parameter body env

eval env (FuncAExp fexp argument) =  do
    funcVal@(FuncVal parameter body oldEnv) <- eval env fexp
    evalArg <- eval env argument
    eval (updateEnv parameter evalArg oldEnv) body

--Others
eval env (NegExp exp) = negateV (eval env exp)
eval env (SqrtExp exp) = sqrtV (eval env exp)
eval env (LDeclExp var val exp) = case eval env val of
    Just x -> eval (updateEnv var x env) exp
    _ -> Nothing

eval env _ = Nothing



