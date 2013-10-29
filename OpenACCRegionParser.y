{
module OpenACCRegionSintatic (parseACCLine) where

import F95VarDecl
import OpenACCRegionLexical

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
  code                          { TokenCodeLine $$ }

%%

Lines   : acc arguments { ACCArgsBegin }
        | acc end arguments { ACCArgsEnd }
        | acc constArguments { ACCConstArgsBegin }
        | acc end constArguments { ACCConstArgsEnds }
        | parameters { ACCParameters }
        | code { CodeLine $1 }

{

data ACCExpr =  CodeLine String
             | ACCArgsBegin
             | ACCArgsEnd
             | ACCConstArgsBegin
             | ACCConstArgsEnds
             | ACCParameters
             deriving (Show)

parseError :: [Token] -> a
parseError _ = error "Sintatic parse error"

-- parseAll :: String -> [ACCExpr]

parseACCLine :: String -> ACCExpr
parseACCLine = parse . scanACCRegionTokens

}
