{
{-# OPTIONS_GHC -w #-}
module OpenACCLexical (Token(..),scanACCTokens) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-

  $eol                              ;
  $white+                           ;
  "#".*                             ;
  \!\$ACC                           { \s -> TokenACC }
  Arguments                         { \s -> TokenArguments }
  ConstArguments                    { \s -> TokenConstArguments }
  \!$white Parameters               { \s -> TokenParamenter }
  End                               { \s -> TokenEnd }
  real                              { \s -> TokenReal }
  integer                           { \s -> TokenInteger }
  kind                              { \s -> TokenKind }
  dimension                         { \s -> TokenDimension }
  parameter                         { \s -> TokenParameter }
  ArgMode                           { \s -> TokenArgMode }
  Read                              { \s -> TokenRead }
  Write                             { \s -> TokenWrite }
  ReadWrite                         { \s -> TokenReadWrite }
  \=                                { \s -> TokenEq }
  \+                                { \s -> TokenAdd }
  \-                                { \s -> TokenSub }
  \(                                { \s -> TokenLParen }
  \)                                { \s -> TokenRParen }
  \:                                { \s -> TokenColon }
  \,                                { \s -> TokenComma }
  $digit+                           { \s -> TokenNumber (read s) }
  $alpha [$alpha $digit \_]*        { \s -> TokenVarName s }

{

data Token = TokenACC
           | TokenArguments
           | TokenConstArguments
           | TokenParamenter
           | TokenEnd
           | TokenConstEnd
           | TokenReal
           | TokenInteger
           | TokenKind
           | TokenDimension
           | TokenParameter
           | TokenArgMode
           | TokenRead
           | TokenWrite
           | TokenReadWrite
           | TokenEq
           | TokenSub
           | TokenAdd
           | TokenLParen
           | TokenRParen
           | TokenColon
           | TokenComma
           | TokenNumber Integer
           | TokenVarName String
           deriving (Eq,Show)

scanACCTokens = alexScanTokens

}
