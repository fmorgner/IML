module IML.MiddleEnd.Productions where

import IML.FrontEnd.Tokens
import IML.MiddleEnd.Parser
import IML.MiddleEnd.Syntax
import IML.MiddleEnd.ProductionHelpers
import Control.Monad
import Control.Applicative

{- Literals -}

literalExpression :: Parser IMLLiteralExpression
literalExpression =  numericLiteral
                 <|> booleanLiteral
                 <|> stringLiteral

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

{- Arithmetic operators -}

binaryArithmeticOperator :: Parser IMLArithmeticOperator
binaryArithmeticOperator =  arithmeticTimes
                        <|> arithmeticDivideBy
                        <|> arithmeticModulo
                        <|> arithmeticPlus
                        <|> arithmeticMinus

arithmeticTimes :: Parser IMLArithmeticOperator
arithmeticTimes = arithOp Times

arithmeticDivideBy :: Parser IMLArithmeticOperator
arithmeticDivideBy = arithOp DivideBy

arithmeticModulo :: Parser IMLArithmeticOperator
arithmeticModulo = arithOp Modulo

arithmeticPlus :: Parser IMLArithmeticOperator
arithmeticPlus = arithOp Plus

arithmeticMinus :: Parser IMLArithmeticOperator
arithmeticMinus = arithOp Minus
