module IML.MiddleEnd.ProductionHelpers where

import IML.FrontEnd.Tokens
import IML.MiddleEnd.Parser
import IML.MiddleEnd.Syntax
import Control.Monad

{-|
Build a parser that expects the given 'Terminal' and returns it on success.
-}
expect :: Terminal -> Parser Token
expect term = (?=?) $ \(Token term' _) -> term' == term

{-|
Build a parser that consumes the given 'Terminal' and returns void on success.
Useful for enforcing the occurrence of a token that does not cary any
information.
-}
consume :: Terminal -> Parser ()
consume ct = void $ expect ct

{-|
Build a parser that expects the given 'IMLArithmeticOperator' and returns it
on success.
-}
arithOp :: IMLArithmeticOperator -> Parser IMLArithmeticOperator
arithOp op = do
  (Token ARITHMETICOPERATOR (Just (ArithmeticOperator op))) <- expect ARITHMETICOPERATOR
  return op

{-|
Build a parser that expects the given 'IMLBooleanOperator' and returns it
on success.
-}
boolOp :: IMLBooleanOperator -> Parser IMLBooleanOperator
boolOp op = do
  (Token BOOLEANOPERATOR (Just (BooleanOperator op))) <- expect BOOLEANOPERATOR
  return op

{-|
Build a parser that expects the given 'IMLRelationalOperator' and returns it
on success.
-}
relaOp :: IMLRelationalOperator -> Parser IMLRelationalOperator
relaOp op = do
  (Token RELATIONALOPERATOR (Just (RelationalOperator op))) <- expect RELATIONALOPERATOR
  return op
