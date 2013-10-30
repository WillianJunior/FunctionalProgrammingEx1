{
module OpenACCRegionSintatic (parseACCLine, Line) where

import F95VarDecl
import OpenACCRegionLexical

}

%name parse
%tokentype { Token }
%error { parseError }

%token

  arguments                     { TokenArguments }
  constArguments                { TokenConstArguments }
  argumentsEnd                  { TokenArgumentsEnd }
  constArgumentsEnd             { TokenConstArgumentsEnd }
  parameters                    { TokenParamenter }
  code                          { TokenCodeLine $$ }

%%

Lines   : arguments { ACCArgsBegin }
        | argumentsEnd { ACCArgsEnd }
        | constArguments { ACCConstArgsBegin }
        | constArgumentsEnd { ACCConstArgsEnds }
        | parameters { ACCParamBegin }
        | code { CodeLine $1 }

{

data Line = ACCArgsBegin
          | ACCArgsEnd
          | ACCConstArgsBegin
          | ACCConstArgsEnds
          | ACCParamBegin
          | CodeLine String
          deriving (Show)

parseError :: [Token] -> a
parseError _ = error "Sintatic parse error"

parseACCLine :: String -> Line
parseACCLine = parse . scanACCRegionTokens

}
