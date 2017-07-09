module IML.MiddleEnd.Productions where

import IML.FrontEnd.Tokens
import IML.MiddleEnd.Parser
import IML.MiddleEnd.Syntax
import Control.Monad

expect :: Terminal -> Parser Token
expect term = (?=?) $ \(Token term' _) -> term' == term

consume :: Terminal -> Parser ()
consume ct = void $ expect ct

numericLiteral :: Parser IMLLiteralExpression
numericLiteral = do
  (Token NUMERIC (Just (ArithmeticValue n))) <- expect NUMERIC
  return (NumericLiteral n)

booleanLiteral :: Parser IMLLiteralExpression
booleanLiteral = do
  (Token BOOLEAN (Just (BooleanValue b))) <- expect BOOLEAN
  return (BooleanLiteral b)

stringLiteral :: Parser IMLLiteralExpression
stringLiteral = do
  (Token STRING (Just (StringValue s))) <- expect STRING
  return (StringLiteral s)
