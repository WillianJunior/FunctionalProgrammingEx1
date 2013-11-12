{
module OpenACCConstParser (parseACCConstLine) where

import F95VarDecl
import OpenACCVarLexical

}

%name parse
%tokentype { Token }
%error { parseError }

%token

  real                          { TokenReal }
  integer                       { TokenInteger }
  kind                          { TokenKind }
  dimension                     { TokenDimension }
  intent                        { TokenIntent }
  argMode                       { TokenArgMode }
  read                          { TokenRead }
  write                         { TokenWrite }
  readWrite                     { TokenReadWrite }
  in                            { TokenIn }
  out                           { TokenOut }
  inout                         { TokenInOut }
  '='                           { TokenEq }
  '+'                           { TokenAdd }
  '-'                           { TokenSub }
  '('                           { TokenLParen }
  ')'                           { TokenRParen }
  ':'                           { TokenColon }
  dbCl                          { TokenDoubleColon }
  ','                           { TokenComma }
  num                           { TokenNumber $$ }
  var                           { TokenVarName $$ }

%%

DeclArg: DataType OptionalKind OptionalDimension OptionalIntent dbCl NameList OptionalArgMode {MkVarDecl (MkVarType $1 $2) $3 $4 (listFlatter $6) $7 False}

DataType: real    { F95Real }
        | integer { F95Integer }

-- kind of const arg optional, if not especified = 4
OptionalKind: {- empty -}           { 4 }
            | '(' kind '=' num ')'  { $4 }

OptionalDimension: {- empty -}                          { [] }
                 | dimension '(' DimensionList ')'  { listFlatter $3 }

DimensionList: Dimension                     { Single $1 }
             | DimensionList ',' Dimension   { Multiple $1 $3 }

Dimension: Expr { MkRange (Const 1) $1 }
         | Expr ':' Expr { MkRange $1 $3 }

Expr: Expr '+' Expr         { Op (MkOpExpr "add" $1 $3) }
    | Expr '-' Expr         { Op (MkOpExpr "sub" $1 $3) }
    | '(' Expr ')'          { $2 }
    | '-' Expr              { Pref (MkPrefixOpExpr "negative" $2) }
    | num                   { Const $1 }
    | var                   { Var $1 }

-- what to do if the intent is not mentioned? is InOut ok?
OptionalIntent: {- empty -}             { InOut }
              | intent '(' Intent ')'   { $3 }

Intent: in    { In }
      | out   { Out }
      | inout { InOut }

NameList: var                 { Single $1 }
        | NameList ',' var    { Multiple $1 $3}

OptionalArgMode: {- empty -}      { ReadWrite }
               | argMode ArgMode  { $2 }


ArgMode: read          { Read }
       | write         { Write }
       | readWrite     { ReadWrite }

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

parseACCConstLine :: String -> VarDecl
parseACCConstLine = parse . scanACCVarTokens

}
