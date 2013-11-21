{-

 Student Identification
 Name: Willian de Oliveira Barreiros Junior
 Matriculation Number: 2105514
 Course: Functional Programming 4
 Exercise Title: Assessed Exercise 1 (Mandatory): 
    Parsing, Code Generation and State Manipulation 
    in Haskell: a Real-world Application
 Date: 21/11/2013

 Status Report
 The code is compiling without any error, and as far as it
 was tested is working. To help the understanding of the
 code, most of the functions have comments.

 One of the recomended ways to deal with exceptions is to
 use the Maybe monad (the same way Map.lookup does it).

 Given that there is the real possibility of an expression
 be inpossible to be evauated (don't have all the parameters),
 this case need to be handled. The first sign that an expression
 can't be evaluated is the return of lookup. If it returns
 Nothing than the expression can't be (fully) evauated. I
 couldn't think on any way to implement eval in a way that
 it would be tolerant to impossible to evaluate expressions
 (at least, not an explicit way to implement exception handling).

 With the use of Maybe, the raise of an exeption is trivial, since
 we can do it with pattern maching, returning Nothing after 
 receiving one.

 My version of eval (myEval) have only one problem: it doesn't
 update the VarTable with every evaluation. Although it doesn't
 keep it from working, if there is a parameter that needs
 to be evaluated before, the evaluation will happen every
 time the parameter is used (inefficient).

 -}

module EvalExpr (eval, myEval, VarTable) where
import F95Types
import qualified Data.Map as H

type VarTable = H.Map String Expr
-- given an expression and the variable lookup table, return the integer value of the evaluated expression and the updated table
eval :: Expr -> VarTable -> (Integer, VarTable)
eval (Var name) vt = eval ((\(Just expr) -> expr) $ H.lookup name vt) vt
eval (Const val) vt = (val, vt)
eval (Op (MkOpExpr "add" lhs rhs)) vt = ((fst $ eval lhs vt) + (fst $ eval rhs vt), vt)
eval (Op (MkOpExpr "sub" lhs rhs)) vt = ((fst $ eval lhs vt) - (fst $ eval rhs vt), vt)
eval (Op (MkOpExpr "div" lhs rhs)) vt = ((fst $ eval lhs vt) `div` (fst $ eval rhs vt), vt)
eval (Op (MkOpExpr "mul" lhs rhs)) vt = ((fst $ eval lhs vt) * (fst $ eval rhs vt), vt)
eval (Pref (MkPrefixOpExpr "negative" expr)) vt = (- (fst $ eval expr vt), vt)

-- given a binary operator expression (e.g. x+y) and the variable lookup table, return the integer value of the evaluated expression
eval_expr :: OpExpr -> VarTable -> Integer -- no clue what to do with it since the operations can be nested (add (sub 3 4) (neg 5)). should return Expr
eval_expr oe vt = 0

-- given a unary operator expression (e.g. -x) and the variable lookup table, return the integer value of the evaluated expression
eval_prefix_expr :: PrefixOpExpr -> VarTable -> Integer
eval_prefix_expr pe vt = 0

-----------------------------------------------------------------------
-- more elegant way to check if an evaluation is possible or not (TODO: an even better way is using Either to perform an incomplete evaluation)
myEval :: Maybe Expr -> VarTable -> Maybe Integer
myEval Nothing _ = Nothing
myEval (Just (Const val)) _ = Just val
myEval (Just (Var name)) vt = myEval (H.lookup name vt) vt
myEval (Just (Op (MkOpExpr "add" lhs rhs))) vt = evalOp (Just lhs) (Just rhs) (+) vt
myEval (Just (Op (MkOpExpr "sub" lhs rhs))) vt = evalOp (Just lhs) (Just rhs) (-) vt
myEval (Just (Op (MkOpExpr "div" lhs rhs))) vt = evalOp (Just lhs) (Just rhs) (div) vt
myEval (Just (Op (MkOpExpr "mul" lhs rhs))) vt = evalOp (Just lhs) (Just rhs) (*) vt
myEval (Just (Pref (MkPrefixOpExpr "negative" expr))) vt = evalOp (Just (Const 0)) (Just expr) (-) vt

fromJust :: Maybe a -> a
fromJust (Just a) = a

-----------------------------------------
-- compute an operation iff both arguments are Just, otherwise return  a Nothing that will propagte
evalOp :: Maybe Expr -> Maybe Expr -> (Integer -> Integer -> Integer) -> VarTable -> Maybe Integer
evalOp (Just lhs) (Just rhs) op vt = do
	if (lhsM /= Nothing && rhsM /= Nothing)
	then Just (op (fromJust lhsM) (fromJust rhsM))
	else Nothing
	where
		lhsM = myEval (Just lhs) vt
		rhsM = myEval (Just rhs) vt
