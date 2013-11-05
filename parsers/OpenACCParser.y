{
module OpenACCSintatic (parseACCLine) where

import F95VarDecl
import OpenACCLexical

}

%name parse
%tokentype { Token }
%error { parseError }

%token

  real                          { TokenReal }
  integer                       { TokenInteger }
  kind                          { TokenKind }
  dimension                     { TokenDimension }
  parameter                     { TokenParameter }
  argMode                       { TokenArgMode }
  read                          { TokenRead }
  write                         { TokenWrite }
  readWrite                     { TokenReadWrite }
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

ACCExpr:  Decl {$1}
        | DeclArg {$1}

Decl: DataType '(' kind '=' num ')' ',' dimension '(' DimensionList ')' dbCl NameList { VarLine (MkVarDecl (MkVarType $1 $5) (listFlatter $10) In (listFlatter $13) Read False) }

DeclArg: DataType '(' kind '=' num ')' ',' dimension '(' DimensionList ')' dbCl NameList acc argMode ArgMode { VarLine (MkVarDecl (MkVarType $1 $5) (listFlatter $10) In (listFlatter $13) $16 False) }

-- kind of const arg optional, if not especified = 4

DataType: real { F95Real }
        | integer { F95Integer }

DimensionList: Dimension { Single $1 }
            | DimensionList ',' Dimension { Multiple $1 $3 }

Dimension: Expr { MkRange (Const 1) $1 }
        | Expr ':' Expr { MkRange $1 $3 }

Expr:   Expr '+' Expr       { Op (MkOpExpr "add" $1 $3) }
    | Expr '-' Expr         { Op (MkOpExpr "sub" $1 $3) }
    | '(' Expr ')'          { $2 }
    | '-' Expr              { Pref (MkPrefixOpExpr "negative" $2) }
    | num                   { Const $1 }
    | var                   { Var $1 }

NameList: var { Single $1 }
        | NameList ',' var { Multiple $1 $3}

ArgMode: read           { Read }
        | write         { Write }
        | readWrite     { ReadWrite }

{

data AbstractList a = Single a
            | Multiple (AbstractList a) a deriving (Show)

ACCLine = VarLine VarDecl
        | ParLine ParDecl

listFlatter :: AbstractList a -> [a]
listFlatter (Single single) = [single]
listFlatter (Multiple list single) = listFlatter list ++ [single]

parseError :: [Token] -> a
parseError _ = error "Sintatic parse error"

parseACCLine :: String -> ACCLine
parseACCLine = parse . scanACCTokens

}
