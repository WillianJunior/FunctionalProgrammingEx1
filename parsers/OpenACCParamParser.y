{
module OpenACCParamParser (parseACCParamLine) where

import F95VarDecl
import F95ParDecl
import OpenACCParamLexical

}

%name parse
%tokentype { Token }
%error { parseError }

%token

  real                          { TokenReal }
  integer                       { TokenInteger }
  kind                          { TokenKind }
  parameter                     { TokenParameter }
  dimension                     { TokenDimension }
  '='                           { TokenEq }
  '+'                           { TokenAdd }
  '-'                           { TokenSub }
  '*'                           { TokenMult }
  '/'                           { TokenDiv }
  '('                           { TokenLParen }
  ')'                           { TokenRParen }
  ':'                           { TokenColon }
  dbCl                          { TokenDoubleColon }
  ','                           { TokenComma }
  num                           { TokenNumber $$ }
  var                           { TokenVarName $$ }

%%

ParamDecl: DataType OptionalKind parameter OptionalDimension dbCl var VariableValue { MkParDecl (MkVarType $1 $2) $4 $6 $7 }

DataType: real    { F95Real }
        | integer { F95Integer }

-- kind of const arg optional, if not especified = 4
OptionalKind: {- empty -}           { 4 }
            | '(' kind '=' num ')'  { $4 }

OptionalDimension: {- empty -}                      { [] }
                 | dimension '(' DimensionList ')'  { listFlatter $3 }

DimensionList: Dimension                     { Single $1 }
             | DimensionList ',' Dimension   { Multiple $1 $3 }

Dimension: Expr          { MkRange (Const 1) $1 }
         | Expr ':' Expr { MkRange $1 $3 }

Expr: Expr '+' Expr         { Op (MkOpExpr "add" $1 $3) }
    | Expr '-' Expr         { Op (MkOpExpr "sub" $1 $3) }
    | Expr '*' Expr         { Op (MkOpExpr "mul" $1 $3) }
    | Expr '/' Expr         { Op (MkOpExpr "div" $1 $3) }
    | '(' Expr ')'          { $2 }
    | '-' Expr              { Pref (MkPrefixOpExpr "negative" $2) }
    | num                   { Const $1 }
    | var                   { Var $1 }

VariableValue: '=' Expr { $2 }

{

data AbstractList a = Single a
                    | Multiple (AbstractList a) a 
                    | Empty deriving (Show)

listFlatter :: AbstractList a -> [a]
listFlatter (Single single) = [single]
listFlatter (Multiple list single) = listFlatter list ++ [single]
listFlatter (Empty) = []

parseError :: [Token] -> a
parseError _ = error "Sintatic parse error"

parseACCParamLine :: String -> ParDecl
parseACCParamLine = parse . scanACCParamTokens

}
