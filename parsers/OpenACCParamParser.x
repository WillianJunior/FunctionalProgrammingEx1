{
{-

 Student Identification
 Name: Willian de Oliveira Barreiros Junior
 Matriculation Number: 2105514
 Course: Functional Programming 4
 Exercise Title: Assessed Exercise 1 (Mandatory): 
    Parsing, Code Generation and State Manipulation 
    in Haskell: a Real-world Application
 Date: 21/11/2013

 Status Report
 The code is compiling without any error, and as far as it
 was tested is working. This file is used to generate the
 parameters lexical scannerusing Alex. The generated code 
 will have only one function for us to use: 
 scanACCParamTokens :: String -> [Token]

 -}

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
