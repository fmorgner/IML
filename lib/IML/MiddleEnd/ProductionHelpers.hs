module IML.MiddleEnd.ProductionHelpers where

import IML.FrontEnd.Tokens
import IML.MiddleEnd.Parser
import IML.MiddleEnd.Syntax
import Control.Monad

expect :: Terminal -> Parser Token
expect term = (?=?) $ \(Token term' _) -> term' == term

consume :: Terminal -> Parser ()
consume ct = void $ expect ct

arithOp :: IMLArithmeticOperator -> Parser IMLArithmeticOperator
arithOp op = do
  (Token ARITHMETICOPERATOR (Just (ArithmeticOperator op))) <- expect ARITHMETICOPERATOR
  return Plus
