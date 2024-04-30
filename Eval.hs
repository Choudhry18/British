module Eval where
import Parser
import Lexer
import Distribution.Compat.Lens (_1)
import GHC.Real (underflowError)
import Data.Text.Array (new)
import Data.Maybe

type Index = Int
type CName = String
data CElem = Method Exp Env | Field Index deriving (Eq, Show)
type CTable = [(Var, CElem)]
type CEnv = [(CName, (Maybe CName, CTable))]
type Env = [(Var, Value)]
data Context = Context {env :: Env, store :: Store, classes :: CEnv} deriving (Eq, Show)
type Location = Integer
type Store = [(Location, Value)]

data Value = IntVal Integer | RealVal Double | StringVal String | BoolVal Bool | PairVal Value Value | UnitVal
             | FuncVal String Exp Env | RefVal Location | ObjectVal String [Value]

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
    show (RefVal loc) = "Error trying to show a reference value"
    show _ = "undefined show"

instance Eq Value where
    (==) :: Value -> Value -> Bool
    (BoolVal val1) == (BoolVal val2) = val1 == val2
    (IntVal val1)  == (IntVal val2) = val1 == val2
    (IntVal val1)  == (RealVal val2) = fromIntegral val1 == val2
    (RealVal val1) == (IntVal val2) = val1 == fromIntegral val2
    (RealVal val1) == (RealVal val2) = val1 == val2
    (StringVal val1) == (StringVal val2) = val1 == val2
    (PairVal lval1 rval1) == (PairVal lval2 rval2) = (lval1 == lval2) && (rval1 == rval2)
    UnitVal == UnitVal = True
    (RefVal val1) == (RefVal val2) = val1 == val2
    _ == _ = False


lookupEnv :: Var -> Env -> Maybe Value
lookupEnv = lookup

lookupStore :: Location -> Store -> Maybe Value
lookupStore = lookup

lookupClass :: CName -> CEnv -> Maybe (Maybe CName, CTable)
lookupClass = lookup 

numFields :: CTable -> Int -> Int
numFields [] cur = cur + 1
numFields (x:xs) cur = case x of 
    (_, Field ind) -> numFields xs (max ind cur)
    (_, _) -> numFields xs cur


newLoc :: Store -> Location
newLoc [] = 0
newLoc sto = (maximum $ map fst sto) + 1


updateEnv :: Var -> Value -> Env -> Env
updateEnv var val env = (var,val) : filter (\(x,_) -> x /= var) env

updateStore :: Location -> Value -> Store -> Store
updateStore loc val store = (loc,val) : filter (\(x,_) -> x /= loc) store

threadStoreLst :: Context -> [Exp] -> Maybe ([Value], Store)
threadStoreLst cont [] = Just ([], store cont)
threadStoreLst cont (e:es) = do
    (ev, st1) <- evalE cont e
    (esv, st2) <- threadStoreLst (cont {store=st1}) es
    Just (ev:esv, st2)

sqrtV :: Value -> Value
sqrtV (RealVal val) = RealVal $ sqrt val
sqrtV (IntVal val) = RealVal $ sqrt $ fromIntegral val

arithOp :: (Integer -> Integer -> Integer) -> (Double -> Double -> Double) -> Value -> Value -> Maybe Value
arithOp f _ (IntVal val1) (IntVal val2) = Just $ IntVal (f val1 val2)
arithOp _ g (RealVal val1) (IntVal val2) = Just $ RealVal (g val1 (fromIntegral val2))
arithOp _ g (IntVal val1) (RealVal val2) = Just $ RealVal (g val2 (fromIntegral val1))
arithOp _ g (RealVal val1) (RealVal val2) = Just $ RealVal (g val1 val2)
arithOp _ _ _ _ = Nothing

compOp :: (Integer -> Integer -> Bool) -> (Double -> Double -> Bool) -> Value -> Value ->  Maybe Value
compOp f g (IntVal val1) (IntVal val2) = Just (BoolVal (f val1 val2))
compOp _ g (RealVal val1) (IntVal val2) = Just (BoolVal (g val1 (fromIntegral val2)))
compOp _ g (IntVal val1) (RealVal val2) = Just (BoolVal (g (fromIntegral val1) val2))
compOp _ g (RealVal val1) (RealVal val2) = Just (BoolVal (g val1 val2))
compOp _ _ _ _ = Nothing

modHelper :: Value -> Value -> Maybe Integer
modHelper val1 val2 = let helper :: Value -> Maybe Integer
                          helper (IntVal val) = Just val
                          helper (RealVal val) = Just $ truncate val
                          helper _ = Nothing
                      in case (helper val1, helper val2) of
                        (Just int1, Just int2) -> Just (int1 `mod` int2)
                        (_ , _ ) -> Nothing


evalD :: Context -> Statement -> Maybe (Value, Context)
evalD context (ExpS exp) = case evalE context exp of
    Just (val, sto) -> Just (val, context {store = sto} )
    Nothing -> Nothing
evalD context (DecS var exp) = do
    (val, nStore) <- evalE context exp
    Just (UnitVal, context{env = updateEnv var val (env context), store = nStore})

-- If this should return nothing in a case or not
evalD context (RecS var exp) =
    let newEnv = updateEnv var newVal (env context)
        newContext = context {env =newEnv, store=sto}
        (newVal, sto) = fromMaybe (UnitVal,store context) (evalE newContext exp)
    in Just (UnitVal, newContext)

evalD context (CDec name elems) = let
    helper :: [CElemD] -> CTable -> Index -> Maybe CTable
    helper [] fields _ = Just fields
    helper (x:xs) fields ind = case x of
        FieldD var -> if any (\(x, _) -> x == var) fields then Nothing else helper xs ((var, Field ind) : fields) (ind + 1)            
        MethodD var body -> if any (\(x, _) -> x == var) fields then Nothing else helper xs ((var, Method body (env context)) : fields) ind
    table = helper elems [] 0
    in case table of 
        Nothing -> Nothing
        Just ctab -> Just (UnitVal, context{classes=(name, (Nothing, ctab)):classes context})

evalD _ _ = Nothing

evalE :: Context -> Exp -> Maybe (Value, Store)
--Arithmetic Expression
evalE context (BinExp AddOp exp1 exp2) = do
    (val1,s1) <- evalE context exp1
    (val2,s2) <- evalE context{store = s1} exp2
    ans <- arithOp (+) (+) val1 val2
    Just (ans, s2)
evalE context (BinExp SubOp exp1 exp2) = do
    (val1,s1) <- evalE context exp1
    (val2,s2) <- evalE context{store = s1} exp2
    ans <- arithOp (-) (-) val1 val2
    Just (ans, s2)
evalE context (BinExp MultOp exp1 exp2) = do
    (val1,s1) <- evalE context exp1
    (val2,s2) <- evalE context{store = s1} exp2
    ans <- arithOp (*) (*) val1 val2
    Just (ans, s2)
evalE context (BinExp ExpOp exp1 exp2) = do
    (val1,s1) <- evalE context exp1
    (val2,s2) <- evalE context{store = s1} exp2
    ans <- arithOp (^) (**) val1 val2
    Just (ans, s2)
evalE context (BinExp ModOp exp1 exp2) = do
    (val1,s1) <- evalE context exp1
    (val2,s2) <- evalE context{store = s1} exp2
    ans <- modHelper val1 val2
    Just (IntVal ans, s2)

evalE context (BinExp DivOp exp1 exp2) = do
    (dividend, s1) <- evalE context exp1
    (divisor, s2) <- evalE context{store = s1} exp2
    remainder <- modHelper dividend divisor
    case (dividend, divisor, remainder) of
        (IntVal x, IntVal y, 0) -> Just (IntVal (x `div` y), s2)
        (IntVal x, RealVal y, 0) -> Just (IntVal (truncate (fromIntegral x/y)), s2)
        (RealVal x, IntVal y, 0) -> Just (IntVal (truncate (x/fromIntegral y)), s2)
        (RealVal x, RealVal y, 0) -> Just (IntVal (truncate (x/y)), s2)
        (RealVal x, RealVal y, _) -> Just (RealVal (x / y), s2)
        (RealVal x, IntVal y, _) -> Just (RealVal (x / fromIntegral y), s2)
        (IntVal x, RealVal y, _) -> Just (RealVal (fromIntegral x / y), s2)
        (IntVal x, IntVal y, _) -> Just (RealVal (fromIntegral x / fromIntegral y), s2)
        (_ , _ , _) -> Nothing

--Comparison Expressions
evalE context (BinExp GOp exp1 exp2) = do
    (val1,s1) <- evalE context exp1
    (val2,s2) <- evalE context{store = s1} exp2
    ans <- compOp (>) (>) val1 val2
    Just (ans, s2)
evalE context (BinExp LOp exp1 exp2) =  do
    (val1,s1) <- evalE context exp1
    (val2,s2) <- evalE context{store = s1} exp2
    ans <- compOp (<) (<) val1 val2
    Just (ans, s2)
evalE context (BinExp GeqOp exp1 exp2) =  do
    (val1,s1) <- evalE context exp1
    (val2,s2) <- evalE context{store = s1} exp2
    ans <- compOp (>=) (>=) val1 val2
    Just (ans, s2)
evalE context (BinExp LeqOp exp1 exp2) =  do
    (val1,s1) <- evalE context exp1
    (val2,s2) <- evalE context{store = s1} exp2
    ans <- compOp (<=) (<=) val1 val2
    Just (ans, s2)
evalE context (BinExp EqOp exp1 exp2) = do
    (val1, s1) <- evalE context exp1
    (val2, s2) <- evalE context{store = s1} exp2
    Just (BoolVal $ val1 == val2, s2)

--Logical Expressions
evalE context (BinExp AndOp exp1 exp2) = case evalE context exp1 of
    Just (BoolVal False, s) -> Just (BoolVal False, s)
    Just (BoolVal True, _)  -> case evalE context exp2 of
        Just (BoolVal False, s) -> Just (BoolVal False, s)
        Just (BoolVal True, s)  -> Just (BoolVal True, s)
        _ -> Nothing
    _ -> Nothing
evalE context (BinExp OrOp exp1 exp2) = case evalE context exp1 of
    Just (BoolVal True, s) -> Just (BoolVal True, s)
    Just (BoolVal False, _) -> case evalE context exp2 of
        Just (BoolVal True, s) -> Just (BoolVal True, s)
        Just (BoolVal False, s) -> Just (BoolVal False, s)
        _ -> Nothing
    _ -> Nothing

--Constants
evalE context (ConstExp Pi) = Just (RealVal pi, store context)
evalE context (ConstExp Fee) = Just (RealVal (exp 1), store context)
evalE context (ConstExp Phi) = Just (RealVal ((sqrt 5 + 2)/2), store context)
evalE context (ConstExp Mole) = Just (RealVal 6.02214076e23, store context)

--Values
evalE context (IntExp val) = Just (IntVal val, store context)
evalE context (RealExp val) = Just (RealVal val, store context)
evalE context (BoolExp val) = Just (BoolVal val, store context)
evalE context (StringExp val) = Just (StringVal val, store context)
evalE context (VarExp val) = case lookupEnv val (env context) of
    Just rval -> Just (rval, store context)
    _ -> Nothing

--Conditional
evalE context (IfExp arg1 arg2 arg3) = case evalE context arg1 of
    Just (IntVal 0, nStore) -> evalE context{store = nStore} arg2
    Just (RealVal 0, nStore) -> evalE context{store = nStore} arg2
    Nothing -> Nothing
    _ -> evalE context arg3
evalE context (HenceExp arg1 arg2 arg3) = case evalE context arg1 of
    Just (BoolVal True, nStore) -> evalE context{store = nStore} arg2
    Just (BoolVal False, nStore) -> evalE context{store = nStore} arg3
    _ -> Nothing

--Pairs
evalE context (PairExp exp1 exp2) = do
    (val1, s1) <- evalE context exp1
    (val2, s2) <-  evalE context{store = s1} exp2
    Just (PairVal val1 val2, s2)

evalE context UnitExp = Just (UnitVal, store context)
evalE context (MateExp exp) = case evalE context exp of
    Just (PairVal ft _, nStore) -> Just (ft, nStore)
    _ -> Nothing
evalE context (BlokeExp exp) = case evalE context exp of
    Just (PairVal _ sd, nStore) -> Just (sd, nStore)
    _ -> Nothing

--Functions
evalE context (FuncDExp parameter body) = Just (FuncVal parameter body (env context), store context)

evalE context (FuncAExp fexp argument) =  do
    funcVal@(FuncVal parameter body oldEnv, _) <- evalE context fexp
    (evalArg,_) <- evalE context argument
    evalE context{env = updateEnv parameter evalArg oldEnv} body

--Others
evalE context (NegExp exp) = do
    (val, s) <- evalE context exp
    case val of
        (IntVal v) -> Just (IntVal $ negate v, s)
        (RealVal v) -> Just (RealVal $ negate v, s)
        _ -> Nothing

evalE context (NegBExp exp) = do
    (val, s) <- evalE context exp
    case val of
        (BoolVal v) -> Just (BoolVal $ not v, s)
        _ -> Nothing

evalE context (SqrtExp exp) = do
    (val, s) <- evalE context exp
    Just (sqrtV val, s)
evalE context (LDeclExp var val exp) = do
    (v, nStore) <- evalE context val
    evalE context{env = updateEnv var v (env context), store = nStore} exp

--Mutability 

evalE context (DisplayExp exp) = do
    (val, sto) <- evalE context exp
    let nLoc = newLoc sto
    Just (RefVal nLoc, (nLoc, val) : sto)

evalE context (DeRefExp exp) = case evalE context exp of
    Just(RefVal locval, newStore) -> do
        val <- lookupStore locval newStore
        Just (val, newStore)
    _ -> Nothing

evalE context (MutExp exp1 exp2) = case evalE context exp1 of
    Just(RefVal locval, s1) -> do
        a <- lookupStore locval s1
        (val, s2) <- evalE context{store = s1} exp2
        Just (val, updateStore locval val s2)
    _ -> Nothing


--Imperative Constructs

evalE context (SeqExp exp1 exp2) = do
    (_, s1) <- evalE context exp1
    (val, s2) <- evalE context{store = s1} exp2
    Just (val, s2)

evalE context (WhileExp cond block) = case evalE context cond of
    Just (BoolVal False, condStore) -> Just (UnitVal, condStore)
    Just (BoolVal True, condStore) -> do
        (_, loopStore) <- evalE (context {store = condStore}) block
        evalE  (context {store = loopStore}) (WhileExp cond block)
    _ -> Nothing

-- Object Oriented 

evalE context (NewExp name exps) = do
    (vals, newStore) <- threadStoreLst context exps
    (_, table) <- lookupClass name (classes context)
    let nfields = numFields table 0
    if length vals == nfields then Just (ObjectVal name vals, newStore) else Nothing

evalE context (LookupExp exp field) = case evalE context exp of
    Just(ObjectVal name values, newStore) -> do
        (_, table) <- lookupClass name (classes context)
        case lookup field table of 
            Just(Field ind) -> case values !! ind of
                val -> Just (val, newStore)
                _ -> Nothing
            Just(Method body mEnv) -> 
                let self = ObjectVal name values
                    selfEnv = ("oneself", self) : mEnv
                in evalE context{env=selfEnv} body
            _ -> Nothing
    _ -> Nothing 

evalE context _ = Nothing



