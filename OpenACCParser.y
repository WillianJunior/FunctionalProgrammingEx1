{
module OpenACCSintatic (parseACCLine) where

import F95VarDecl
import OpenACCLexical

}

%name parse
%tokentype { Token }
%error { parseError }

%token

  acc                           { TokenACC }
  arguments                     { TokenArguments }
  constArguments                { TokenConstArguments }
  parameters                    { TokenParamenter }
  end                           { TokenEnd }
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
  ','                           { TokenComma }
  num                           { TokenNumber $$ }
  var                           { TokenVarName $$ }

%%

ACCExpr: ACCArgs {$1}
        | ACCArgsEnd {$1}
        | ACCConstArgs {$1}
        | ACCConstArgsEnds {$1}
        | Decl {$1}
        | DeclArg {$1}

ACCArgs: acc arguments { ACCArgsBegin }

ACCArgsEnd: acc end arguments { ACCArgsEnd }

ACCConstArgs: acc constArguments { ACCConstArgsBegin }

ACCConstArgsEnds: acc end constArguments { ACCConstArgsEnds }

Decl: DataType '(' kind '=' num ')' ',' dimension '(' DimensionList ')' ':'':' NameList { ACCVarDecl (MkVarDecl (MkVarType $1 $5) (listFlatter $10) In (listFlatter $14) Read False) }

DeclArg: DataType '(' kind '=' num ')' ',' dimension '(' DimensionList ')' ':'':' NameList acc argMode ArgMode { ACCVarDecl (MkVarDecl (MkVarType $1 $5) (listFlatter $10) In (listFlatter $14) $17 False) }

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

data ACCExpr =  ACCVarDecl VarDecl
             | ACCArgsEnd
             | ACCArgsBegin
             | ACCConstArgsBegin
             | ACCConstArgsEnds
             deriving (Show)

listFlatter :: AbstractList a -> [a]
listFlatter (Single single) = [single]
listFlatter (Multiple list single) = listFlatter list ++ [single]

parseError :: [Token] -> a
parseError _ = error "Sintatic parse error"

-- parseAll :: String -> [ACCExpr]

parseACCLine :: String -> ACCExpr
parseACCLine = parse . scanACCTokens

}
