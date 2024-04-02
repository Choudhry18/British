{-# OPTIONS_GHC -w #-}
module Parser where
import Lexer
import Data.Maybe
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,97) ([20608,234,54433,16257,64,21124,7,0,18960,8221,14996,10304,117,0,0,0,0,0,0,766,4097,7498,37920,16442,29992,20608,234,54433,16897,937,21124,7,256,0,0,0,2032,57346,143,64,0,43330,3,0,0,48128,0,56,61440,2,224,16384,0,128,0,0,65024,16,18960,63517,3,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","S","E","\"=\"","\"+\"","\"-\"","\"*\"","\"^\"","\"/\"","\"%\"","mr","ms","ifz","then","else","\"(\"","\")\"","sqrt","eol","const","int","real","%eof"]
        bit_start = st Prelude.* 25
        bit_end = (st Prelude.+ 1) Prelude.* 25
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..24]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (8) = happyShift action_3
action_0 (13) = happyShift action_4
action_0 (15) = happyShift action_5
action_0 (18) = happyShift action_6
action_0 (20) = happyShift action_7
action_0 (22) = happyShift action_8
action_0 (23) = happyShift action_9
action_0 (24) = happyShift action_10
action_0 (4) = happyGoto action_11
action_0 (5) = happyGoto action_12
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (8) = happyShift action_3
action_1 (13) = happyShift action_4
action_1 (15) = happyShift action_5
action_1 (18) = happyShift action_6
action_1 (20) = happyShift action_7
action_1 (22) = happyShift action_8
action_1 (23) = happyShift action_9
action_1 (24) = happyShift action_10
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (6) = happyShift action_13
action_2 (7) = happyShift action_14
action_2 (8) = happyShift action_15
action_2 (9) = happyShift action_16
action_2 (10) = happyShift action_17
action_2 (11) = happyShift action_18
action_2 (12) = happyShift action_19
action_2 (21) = happyShift action_21
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (8) = happyShift action_3
action_3 (13) = happyShift action_4
action_3 (15) = happyShift action_5
action_3 (18) = happyShift action_6
action_3 (20) = happyShift action_7
action_3 (22) = happyShift action_8
action_3 (23) = happyShift action_9
action_3 (24) = happyShift action_10
action_3 (5) = happyGoto action_25
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_17

action_5 (8) = happyShift action_3
action_5 (13) = happyShift action_4
action_5 (15) = happyShift action_5
action_5 (18) = happyShift action_6
action_5 (20) = happyShift action_7
action_5 (22) = happyShift action_8
action_5 (23) = happyShift action_9
action_5 (24) = happyShift action_10
action_5 (5) = happyGoto action_24
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (8) = happyShift action_3
action_6 (13) = happyShift action_4
action_6 (15) = happyShift action_5
action_6 (18) = happyShift action_6
action_6 (20) = happyShift action_7
action_6 (22) = happyShift action_8
action_6 (23) = happyShift action_9
action_6 (24) = happyShift action_10
action_6 (5) = happyGoto action_23
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (8) = happyShift action_3
action_7 (13) = happyShift action_4
action_7 (15) = happyShift action_5
action_7 (18) = happyShift action_6
action_7 (20) = happyShift action_7
action_7 (22) = happyShift action_8
action_7 (23) = happyShift action_9
action_7 (24) = happyShift action_10
action_7 (5) = happyGoto action_22
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_5

action_9 _ = happyReduce_3

action_10 _ = happyReduce_4

action_11 (25) = happyAccept
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (6) = happyShift action_13
action_12 (7) = happyShift action_14
action_12 (8) = happyShift action_15
action_12 (9) = happyShift action_16
action_12 (10) = happyShift action_17
action_12 (11) = happyShift action_18
action_12 (12) = happyShift action_19
action_12 (14) = happyShift action_20
action_12 (21) = happyShift action_21
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (8) = happyShift action_3
action_13 (13) = happyShift action_4
action_13 (15) = happyShift action_5
action_13 (18) = happyShift action_6
action_13 (20) = happyShift action_7
action_13 (22) = happyShift action_8
action_13 (23) = happyShift action_9
action_13 (24) = happyShift action_10
action_13 (5) = happyGoto action_35
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (8) = happyShift action_3
action_14 (13) = happyShift action_4
action_14 (15) = happyShift action_5
action_14 (18) = happyShift action_6
action_14 (20) = happyShift action_7
action_14 (22) = happyShift action_8
action_14 (23) = happyShift action_9
action_14 (24) = happyShift action_10
action_14 (5) = happyGoto action_34
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (8) = happyShift action_3
action_15 (13) = happyShift action_4
action_15 (15) = happyShift action_5
action_15 (18) = happyShift action_6
action_15 (20) = happyShift action_7
action_15 (22) = happyShift action_8
action_15 (23) = happyShift action_9
action_15 (24) = happyShift action_10
action_15 (5) = happyGoto action_33
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (8) = happyShift action_3
action_16 (13) = happyShift action_4
action_16 (15) = happyShift action_5
action_16 (18) = happyShift action_6
action_16 (20) = happyShift action_7
action_16 (22) = happyShift action_8
action_16 (23) = happyShift action_9
action_16 (24) = happyShift action_10
action_16 (5) = happyGoto action_32
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (8) = happyShift action_3
action_17 (13) = happyShift action_4
action_17 (15) = happyShift action_5
action_17 (18) = happyShift action_6
action_17 (20) = happyShift action_7
action_17 (22) = happyShift action_8
action_17 (23) = happyShift action_9
action_17 (24) = happyShift action_10
action_17 (5) = happyGoto action_31
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (8) = happyShift action_3
action_18 (13) = happyShift action_4
action_18 (15) = happyShift action_5
action_18 (18) = happyShift action_6
action_18 (20) = happyShift action_7
action_18 (22) = happyShift action_8
action_18 (23) = happyShift action_9
action_18 (24) = happyShift action_10
action_18 (5) = happyGoto action_30
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (8) = happyShift action_3
action_19 (13) = happyShift action_4
action_19 (15) = happyShift action_5
action_19 (18) = happyShift action_6
action_19 (20) = happyShift action_7
action_19 (22) = happyShift action_8
action_19 (23) = happyShift action_9
action_19 (24) = happyShift action_10
action_19 (5) = happyGoto action_29
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (21) = happyShift action_28
action_20 _ = happyFail (happyExpListPerState 20)

action_21 _ = happyReduce_1

action_22 _ = happyReduce_8

action_23 (6) = happyShift action_13
action_23 (7) = happyShift action_14
action_23 (8) = happyShift action_15
action_23 (9) = happyShift action_16
action_23 (10) = happyShift action_17
action_23 (11) = happyShift action_18
action_23 (12) = happyShift action_19
action_23 (19) = happyShift action_27
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (6) = happyShift action_13
action_24 (7) = happyShift action_14
action_24 (8) = happyShift action_15
action_24 (9) = happyShift action_16
action_24 (10) = happyShift action_17
action_24 (11) = happyShift action_18
action_24 (12) = happyShift action_19
action_24 (16) = happyShift action_26
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (6) = happyShift action_13
action_25 _ = happyReduce_7

action_26 (8) = happyShift action_3
action_26 (13) = happyShift action_4
action_26 (15) = happyShift action_5
action_26 (18) = happyShift action_6
action_26 (20) = happyShift action_7
action_26 (22) = happyShift action_8
action_26 (23) = happyShift action_9
action_26 (24) = happyShift action_10
action_26 (5) = happyGoto action_36
action_26 _ = happyFail (happyExpListPerState 26)

action_27 _ = happyReduce_6

action_28 _ = happyReduce_2

action_29 (6) = happyShift action_13
action_29 (7) = happyShift action_14
action_29 (8) = happyShift action_15
action_29 (9) = happyShift action_16
action_29 (11) = happyShift action_18
action_29 _ = happyReduce_15

action_30 (6) = happyShift action_13
action_30 (7) = happyShift action_14
action_30 (8) = happyShift action_15
action_30 _ = happyReduce_11

action_31 (6) = happyShift action_13
action_31 (7) = happyShift action_14
action_31 (8) = happyShift action_15
action_31 (9) = happyShift action_16
action_31 (11) = happyShift action_18
action_31 _ = happyReduce_13

action_32 (6) = happyShift action_13
action_32 (7) = happyShift action_14
action_32 (8) = happyShift action_15
action_32 _ = happyReduce_10

action_33 (6) = happyShift action_13
action_33 _ = happyReduce_14

action_34 (6) = happyShift action_13
action_34 _ = happyReduce_9

action_35 _ = happyReduce_12

action_36 (6) = happyShift action_13
action_36 (7) = happyShift action_14
action_36 (8) = happyShift action_15
action_36 (9) = happyShift action_16
action_36 (10) = happyShift action_17
action_36 (11) = happyShift action_18
action_36 (12) = happyShift action_19
action_36 (17) = happyShift action_37
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (8) = happyShift action_3
action_37 (13) = happyShift action_4
action_37 (15) = happyShift action_5
action_37 (18) = happyShift action_6
action_37 (20) = happyShift action_7
action_37 (22) = happyShift action_8
action_37 (23) = happyShift action_9
action_37 (24) = happyShift action_10
action_37 (5) = happyGoto action_38
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (6) = happyShift action_13
action_38 (7) = happyShift action_14
action_38 (8) = happyShift action_15
action_38 (9) = happyShift action_16
action_38 (10) = happyShift action_17
action_38 (11) = happyShift action_18
action_38 (12) = happyShift action_19
action_38 _ = happyReduce_16

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 _
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (ExpS happy_var_1
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  4 happyReduction_2
happyReduction_2 _
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (MsS happy_var_1
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyTerminal (IntTok happy_var_1))
	 =  HappyAbsSyn5
		 (IntExp happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  5 happyReduction_4
happyReduction_4 (HappyTerminal (Realtok happy_var_1))
	 =  HappyAbsSyn5
		 (RealExp happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  5 happyReduction_5
happyReduction_5 (HappyTerminal (ConstTok happy_var_1))
	 =  HappyAbsSyn5
		 (ConstExp happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  5 happyReduction_6
happyReduction_6 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  5 happyReduction_7
happyReduction_7 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (NegExp happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  5 happyReduction_8
happyReduction_8 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (SqrtExp happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  5 happyReduction_9
happyReduction_9 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (BinExp AddOp happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  5 happyReduction_10
happyReduction_10 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (BinExp MultOp happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  5 happyReduction_11
happyReduction_11 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (BinExp DivOp happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  5 happyReduction_12
happyReduction_12 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (BinExp EqOp happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  5 happyReduction_13
happyReduction_13 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (BinExp ExpOp happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  5 happyReduction_14
happyReduction_14 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (BinExp SubOp happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  5 happyReduction_15
happyReduction_15 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (BinExp ModOp happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happyReduce 6 5 happyReduction_16
happyReduction_16 ((HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (IfExp happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_1  5 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn5
		 (MrExp
	)

happyNewToken action sts stk [] =
	action 25 25 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	OpTok EqOp -> cont 6;
	OpTok PlusOp -> cont 7;
	OpTok SubOp -> cont 8;
	OpTok MultOp -> cont 9;
	OpTok ExpOp -> cont 10;
	OpTok DivOp -> cont 11;
	OpTok ModOp -> cont 12;
	MRTok -> cont 13;
	MSTok -> cont 14;
	IfzTok -> cont 15;
	ThenTok -> cont 16;
	ElseTok -> cont 17;
	LeftPTok -> cont 18;
	RightPTok -> cont 19;
	SqrtTok -> cont 20;
	EOLTok -> cont 21;
	ConstTok happy_dollar_dollar -> cont 22;
	IntTok happy_dollar_dollar -> cont 23;
	Realtok happy_dollar_dollar -> cont 24;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 25 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Maybe a -> (a -> Maybe b) -> Maybe b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> Maybe a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Maybe a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Maybe a
happyError' = (\(tokens, _) -> parseError tokens)
parse tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


data Statement = ExpS Exp | MsS Exp  
data Exp = IntExp Integer | RealExp Double | ConstExp Const | SqrtExp Exp | BinExp Op Exp Exp | IfExp Exp Exp Exp | MrExp 
           |NegExp Exp deriving (Show, Eq)

parseError :: [Token] -> Maybe a
parseError _ = Nothing

expOfStr str = parse =<< scanTokens str
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
