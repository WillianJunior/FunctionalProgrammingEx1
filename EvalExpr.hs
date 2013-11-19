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

evalOp :: Maybe Expr -> Maybe Expr -> (Integer -> Integer -> Integer) -> VarTable -> Maybe Integer
evalOp (Just lhs) (Just rhs) op vt = do
	if (lhsM /= Nothing && rhsM /= Nothing)
	then Just (op (fromJust lhsM) (fromJust rhsM))
	else Nothing
	where
		lhsM = myEval (Just lhs) vt
		rhsM = myEval (Just rhs) vt
