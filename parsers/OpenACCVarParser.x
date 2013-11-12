{
{-# OPTIONS_GHC -w #-}
module OpenACCVarLexical (Token(..),scanACCVarTokens) where
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
  \,[$white]*dimension              { \s -> TokenDimension }
  \,[$white]*intent                 { \s -> TokenIntent }
  "!$ACC Argmode"                   { \s -> TokenArgMode }
  Read                              { \s -> TokenRead }
  Write                             { \s -> TokenWrite }
  ReadWrite                         { \s -> TokenReadWrite }
  in                                { \s -> TokenIn }
  out                               { \s -> TokenOut }
  inout                             { \s -> TokenInOut }
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

data Token = TokenACC
           | TokenReal
           | TokenInteger
           | TokenKind
           | TokenDimension
           | TokenIntent
           | TokenArgMode
           | TokenRead
           | TokenWrite
           | TokenReadWrite
           | TokenIn
           | TokenOut
           | TokenInOut
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

scanACCVarTokens = alexScanTokens

}
