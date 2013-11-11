{-# OPTIONS_GHC -w #-}
module OpenACCVarParser (parseACCVarLine) where

import F95VarDecl
import OpenACCVarLexical

-- parser produced by Happy Version 1.18.9

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14

action_0 (15) = happyShift action_4
action_0 (16) = happyShift action_5
action_0 (4) = happyGoto action_6
action_0 (7) = happyGoto action_2
action_0 (8) = happyGoto action_7
action_0 (9) = happyGoto action_8
action_0 _ = happyFail

action_1 (15) = happyShift action_4
action_1 (16) = happyShift action_5
action_1 (7) = happyGoto action_2
action_1 (9) = happyGoto action_3
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 (27) = happyShift action_10
action_3 _ = happyFail

action_4 _ = happyReduce_8

action_5 _ = happyReduce_9

action_6 (34) = happyAccept
action_6 _ = happyFail

action_7 _ = happyReduce_2

action_8 (27) = happyShift action_9
action_8 _ = happyFail

action_9 (17) = happyShift action_12
action_9 _ = happyFail

action_10 (17) = happyShift action_11
action_10 _ = happyFail

action_11 (24) = happyShift action_14
action_11 _ = happyFail

action_12 (24) = happyShift action_13
action_12 _ = happyFail

action_13 (32) = happyShift action_16
action_13 _ = happyFail

action_14 (32) = happyShift action_15
action_14 _ = happyFail

action_15 (28) = happyShift action_18
action_15 _ = happyFail

action_16 (28) = happyShift action_17
action_16 _ = happyFail

action_17 (31) = happyShift action_20
action_17 _ = happyFail

action_18 (31) = happyShift action_19
action_18 _ = happyFail

action_19 (18) = happyShift action_22
action_19 _ = happyFail

action_20 (18) = happyShift action_21
action_20 _ = happyFail

action_21 (27) = happyShift action_24
action_21 _ = happyFail

action_22 (27) = happyShift action_23
action_22 _ = happyFail

action_23 (26) = happyShift action_28
action_23 (27) = happyShift action_29
action_23 (32) = happyShift action_30
action_23 (33) = happyShift action_31
action_23 (10) = happyGoto action_32
action_23 (11) = happyGoto action_26
action_23 (12) = happyGoto action_27
action_23 _ = happyFail

action_24 (26) = happyShift action_28
action_24 (27) = happyShift action_29
action_24 (32) = happyShift action_30
action_24 (33) = happyShift action_31
action_24 (10) = happyGoto action_25
action_24 (11) = happyGoto action_26
action_24 (12) = happyGoto action_27
action_24 _ = happyReduce_12

action_25 (28) = happyShift action_40
action_25 (31) = happyShift action_34
action_25 _ = happyFail

action_26 _ = happyReduce_10

action_27 (25) = happyShift action_37
action_27 (26) = happyShift action_38
action_27 (29) = happyShift action_39
action_27 _ = happyReduce_13

action_28 (26) = happyShift action_28
action_28 (27) = happyShift action_29
action_28 (32) = happyShift action_30
action_28 (33) = happyShift action_31
action_28 (12) = happyGoto action_36
action_28 _ = happyFail

action_29 (26) = happyShift action_28
action_29 (27) = happyShift action_29
action_29 (32) = happyShift action_30
action_29 (33) = happyShift action_31
action_29 (12) = happyGoto action_35
action_29 _ = happyFail

action_30 _ = happyReduce_19

action_31 _ = happyReduce_20

action_32 (28) = happyShift action_33
action_32 (31) = happyShift action_34
action_32 _ = happyFail

action_33 (30) = happyShift action_47
action_33 _ = happyFail

action_34 (26) = happyShift action_28
action_34 (27) = happyShift action_29
action_34 (32) = happyShift action_30
action_34 (33) = happyShift action_31
action_34 (11) = happyGoto action_46
action_34 (12) = happyGoto action_27
action_34 _ = happyFail

action_35 (25) = happyShift action_37
action_35 (26) = happyShift action_38
action_35 (28) = happyShift action_45
action_35 _ = happyFail

action_36 (25) = happyShift action_37
action_36 (26) = happyShift action_38
action_36 _ = happyReduce_18

action_37 (26) = happyShift action_28
action_37 (27) = happyShift action_29
action_37 (32) = happyShift action_30
action_37 (33) = happyShift action_31
action_37 (12) = happyGoto action_44
action_37 _ = happyFail

action_38 (26) = happyShift action_28
action_38 (27) = happyShift action_29
action_38 (32) = happyShift action_30
action_38 (33) = happyShift action_31
action_38 (12) = happyGoto action_43
action_38 _ = happyFail

action_39 (26) = happyShift action_28
action_39 (27) = happyShift action_29
action_39 (32) = happyShift action_30
action_39 (33) = happyShift action_31
action_39 (12) = happyGoto action_42
action_39 _ = happyFail

action_40 (30) = happyShift action_41
action_40 _ = happyFail

action_41 (33) = happyShift action_49
action_41 (13) = happyGoto action_50
action_41 _ = happyFail

action_42 (25) = happyShift action_37
action_42 (26) = happyShift action_38
action_42 _ = happyReduce_14

action_43 (25) = happyShift action_37
action_43 (26) = happyShift action_38
action_43 _ = happyReduce_16

action_44 (25) = happyShift action_37
action_44 (26) = happyShift action_38
action_44 _ = happyReduce_15

action_45 _ = happyReduce_17

action_46 _ = happyReduce_11

action_47 (33) = happyShift action_49
action_47 (13) = happyGoto action_48
action_47 _ = happyFail

action_48 (31) = happyShift action_52
action_48 _ = happyFail

action_49 _ = happyReduce_21

action_50 (20) = happyShift action_51
action_50 (31) = happyShift action_52
action_50 _ = happyReduce_6

action_51 (21) = happyShift action_55
action_51 (22) = happyShift action_56
action_51 (23) = happyShift action_57
action_51 (14) = happyGoto action_54
action_51 _ = happyFail

action_52 (33) = happyShift action_53
action_52 _ = happyFail

action_53 _ = happyReduce_22

action_54 _ = happyReduce_7

action_55 _ = happyReduce_23

action_56 _ = happyReduce_24

action_57 _ = happyReduce_25

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happyReduce 9 5 happyReduction_3
happyReduction_3 ((HappyAbsSyn13  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenNumber happy_var_5)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (MkVarDecl (MkVarType happy_var_1 happy_var_5) happy_var_7 In (listFlatter happy_var_9) Read False
	) `HappyStk` happyRest

happyReduce_4 = happySpecReduce_0  6 happyReduction_4
happyReduction_4  =  HappyAbsSyn6
		 (
	)

happyReduce_5 = happyReduce 5 6 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (listFlatter happy_var_4
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 13 7 happyReduction_6
happyReduction_6 ((HappyAbsSyn13  happy_var_13) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenNumber happy_var_5)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (MkVarDecl (MkVarType happy_var_1 happy_var_5) (listFlatter happy_var_10) In (listFlatter happy_var_13) Read False
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 15 8 happyReduction_7
happyReduction_7 ((HappyAbsSyn14  happy_var_15) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_13) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenNumber happy_var_5)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (MkVarDecl (MkVarType happy_var_1 happy_var_5) (listFlatter happy_var_10) In (listFlatter happy_var_13) happy_var_15 False
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_1  9 happyReduction_8
happyReduction_8 _
	 =  HappyAbsSyn9
		 (F95Real
	)

happyReduce_9 = happySpecReduce_1  9 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn9
		 (F95Integer
	)

happyReduce_10 = happySpecReduce_1  10 happyReduction_10
happyReduction_10 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (Single happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  10 happyReduction_11
happyReduction_11 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Multiple happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_0  10 happyReduction_12
happyReduction_12  =  HappyAbsSyn10
		 (Empty
	)

happyReduce_13 = happySpecReduce_1  11 happyReduction_13
happyReduction_13 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (MkRange (Const 1) happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  11 happyReduction_14
happyReduction_14 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (MkRange happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  12 happyReduction_15
happyReduction_15 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Op (MkOpExpr "add" happy_var_1 happy_var_3)
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  12 happyReduction_16
happyReduction_16 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Op (MkOpExpr "sub" happy_var_1 happy_var_3)
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  12 happyReduction_17
happyReduction_17 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_2  12 happyReduction_18
happyReduction_18 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (Pref (MkPrefixOpExpr "negative" happy_var_2)
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  12 happyReduction_19
happyReduction_19 (HappyTerminal (TokenNumber happy_var_1))
	 =  HappyAbsSyn12
		 (Const happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  12 happyReduction_20
happyReduction_20 (HappyTerminal (TokenVarName happy_var_1))
	 =  HappyAbsSyn12
		 (Var happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  13 happyReduction_21
happyReduction_21 (HappyTerminal (TokenVarName happy_var_1))
	 =  HappyAbsSyn13
		 (Single happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  13 happyReduction_22
happyReduction_22 (HappyTerminal (TokenVarName happy_var_3))
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Multiple happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  14 happyReduction_23
happyReduction_23 _
	 =  HappyAbsSyn14
		 (Read
	)

happyReduce_24 = happySpecReduce_1  14 happyReduction_24
happyReduction_24 _
	 =  HappyAbsSyn14
		 (Write
	)

happyReduce_25 = happySpecReduce_1  14 happyReduction_25
happyReduction_25 _
	 =  HappyAbsSyn14
		 (ReadWrite
	)

happyNewToken action sts stk [] =
	action 34 34 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenReal -> cont 15;
	TokenInteger -> cont 16;
	TokenKind -> cont 17;
	TokenDimension -> cont 18;
	TokenParameter -> cont 19;
	TokenArgMode -> cont 20;
	TokenRead -> cont 21;
	TokenWrite -> cont 22;
	TokenReadWrite -> cont 23;
	TokenEq -> cont 24;
	TokenAdd -> cont 25;
	TokenSub -> cont 26;
	TokenLParen -> cont 27;
	TokenRParen -> cont 28;
	TokenColon -> cont 29;
	TokenDoubleColon -> cont 30;
	TokenComma -> cont 31;
	TokenNumber happy_dollar_dollar -> cont 32;
	TokenVarName happy_dollar_dollar -> cont 33;
	_ -> happyError' (tk:tks)
	}

happyError_ 34 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


data AbstractList a = Single a
                    | Multiple (AbstractList a) a 
                    | Empty deriving (Show)

listFlatter :: AbstractList a -> [a]
listFlatter (Single single) = [single]
listFlatter (Multiple list single) = listFlatter list ++ [single]
listFlatter (Empty) = []

parseError :: [Token] -> a
parseError _ = error "Sintatic parse error"

parseACCVarLine :: String -> VarDecl
parseACCVarLine = parse . scanACCVarTokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 312 "templates/GenericTemplate.hs" #-}
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
