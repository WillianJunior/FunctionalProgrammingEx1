{
{-# OPTIONS_GHC -w #-}
module OpenACCRegions (Line(..),scanACCRegions) where
}

%wrapper "basic"

$eol   = [\n]
$digit   = 0-9
$alpha   = [a-zA-Z]
$symbols = [\.\:\=\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/\!\_\&\']

tokens :-

  "!$ACC Arguments"                        { \s -> ACCArgsBegin }
  "!$ACC ConstArguments"                   { \s -> ACCArgsEnd }
  "! Parameters"                           { \s -> ACCConstArgsBegin }
  "!$ACC End Arguments"                    { \s -> ACCConstArgsEnds }
  "!$ACC End ConstArguments"               { \s -> ACCParamBegin }
  [$alpha $digit $symbols $white]+         { \s -> CodeLine s }

{

data Line = ACCArgsBegin
          | ACCArgsEnd
          | ACCConstArgsBegin
          | ACCConstArgsEnds
          | ACCParamBegin
          | CodeLine String
          deriving (Show)

scanACCRegions = alexScanTokens

}
