module EvalExpr (eval, myEval, VarTable) where
import F95Types
import qualified Data.Map as H

type VarTable = H.Map String Expr
-- given an expression and the variable lookup table, return the integer value of the evaluated expression and the updated table
eval :: Expr -> VarTable -> (Integer, VarTable)
eval expr vtable = (0,H.empty)

-- given a binary operator expression (e.g. x+y) and the variable lookup table, return the integer value of the evaluated expression
eval_expr :: OpExpr -> VarTable -> Integer -- no clue what to do with it since the operations can be nested (add (sub 3 4) (neg 5)). should return Expr
eval_expr oe vt = 0

-- given a unary operator expression (e.g. -x) and the variable lookup table, return the integer value of the evaluated expression
eval_prefix_expr :: PrefixOpExpr -> VarTable -> Integer
eval_prefix_expr pe vt = 0

myEval :: Expr -> VarTable -> Integer -- this can evolve into expr (what if the var is an expression? we should only calculate it once)
myEval (Var name) vt = myEval ((\(Just expr) -> expr) $ H.lookup name vt) vt
myEval (Const val) _ = val
myEval (Op (MkOpExpr "add" lhs rhs)) vt = (myEval lhs vt) + (myEval rhs vt)
myEval (Op (MkOpExpr "sub" lhs rhs)) vt = (myEval lhs vt) - (myEval rhs vt)
myEval (Op (MkOpExpr "div" lhs rhs)) vt = (myEval lhs vt) `div` (myEval rhs vt)
myEval (Op (MkOpExpr "mul" lhs rhs)) vt = (myEval lhs vt) * (myEval rhs vt)
myEval (Pref (MkPrefixOpExpr "negative" expr)) vt = - myEval expr vt