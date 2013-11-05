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
  real                              { \s -> TokenReal }
  integer                           { \s -> TokenInteger }
  kind                              { \s -> TokenKind }
  dimension                         { \s -> TokenDimension }
  parameter                         { \s -> TokenParameter }
  "!$ACC ArgMode"                   { \s -> TokenArgMode }
  Read                              { \s -> TokenRead }
  Write                             { \s -> TokenWrite }
  ReadWrite                         { \s -> TokenReadWrite }
  \=                                { \s -> TokenEq }
  \+                                { \s -> TokenAdd }
  \-                                { \s -> TokenSub }
  \(                                { \s -> TokenLParen }
  \)                                { \s -> TokenRParen }
  \:                                { \s -> TokenColon }
  "::"                              { \s -> TokenDoubleColon }
  \,                                { \s -> TokenComma }
  $digit+                           { \s -> TokenNumber (read s) }
  $alpha [$alpha $digit \_]*        { \s -> TokenVarName s }

{

data Token = TokenReal
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
           | TokenDoubleColon
           | TokenComma
           | TokenNumber Integer
           | TokenVarName String
           deriving (Eq,Show)

scanACCTokens = alexScanTokens

}
