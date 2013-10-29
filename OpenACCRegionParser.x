{
{-# OPTIONS_GHC -w #-}
module OpenACCRegionLexical (Token(..),scanACCRegionTokens) where
}

%wrapper "basic"

$eol   = [\n]
$digit   = 0-9
$alpha   = [a-zA-Z]
$symbols = [\.\:\=\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/\!\_\&\']

tokens :-

  "!$ACC Arguments"                        { \s -> TokenArguments }
  "!$ACC ConstArguments"                   { \s -> TokenConstArguments }
  "! Parameters"                           { \s -> TokenParamenter }
  "!$ACC End Arguments"                    { \s -> TokenArgumentsEnd }
  "!$ACC End ConstArguments"               { \s -> TokenConstArgumentsEnd }
  [$alpha $digit $symbols $white]+         { \s -> TokenCodeLine s }

{

data Token = TokenACC
           | TokenArguments
           | TokenConstArguments
           | TokenParamenter
           | TokenArgumentsEnd
           | TokenConstArgumentsEnd
           | TokenCodeLine String
           deriving (Eq,Show)

scanACCRegionTokens = alexScanTokens

}
