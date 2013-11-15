{
{-# OPTIONS_GHC -w #-}
module OpenACCParamLexical (Token(..),scanACCParamTokens) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-

  $white+                           ;
  real                              { \s -> TokenReal }
  integer                           { \s -> TokenInteger }
  kind                              { \s -> TokenKind }
  \,[$white]*parameter              { \s -> TokenParameter }
  \,[$white]*dimension              { \s -> TokenDimension }
  \=                                { \s -> TokenEq }
  \+                                { \s -> TokenAdd }
  \-                                { \s -> TokenSub }
  \*                                { \s -> TokenMult }
  \/                                { \s -> TokenDiv }
  \(                                { \s -> TokenLParen }
  \)                                { \s -> TokenRParen }
  \:                                { \s -> TokenColon }
  "::"                              { \s -> TokenDoubleColon }
  \,                                { \s -> TokenComma }
  $digit+                           { \s -> TokenNumber (read s) }
  $alpha [$alpha $digit \_]*        { \s -> TokenVarName s }

{

data Token = TokenACC
           | TokenReal
           | TokenInteger
           | TokenKind
           | TokenParameter
           | TokenDimension
           | TokenEq
           | TokenSub
           | TokenAdd
           | TokenMult
           | TokenDiv
           | TokenLParen
           | TokenRParen
           | TokenColon
           | TokenDoubleColon
           | TokenComma
           | TokenNumber Integer
           | TokenVarName String
           deriving (Eq,Show)

scanACCParamTokens = alexScanTokens

}
