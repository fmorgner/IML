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
Build a parser that expects the given 'IMLAdditiveOperator' and returns it
on success.
-}
additiveOp :: IMLAdditiveOperator -> Parser IMLAdditiveOperator
additiveOp op = do
  (Token ARITHMETICOPERATOR (Just (AdditiveOperator op))) <- expect ARITHMETICOPERATOR
  return op

{-|
Build a parser that expects the given 'IMLMultiplicativeOperator' and returns it
on success.
-}
multiplicativeOp :: IMLMultiplicativeOperator -> Parser IMLMultiplicativeOperator
multiplicativeOp op = do
  (Token ARITHMETICOPERATOR (Just (MultiplicativeOperator op))) <- expect ARITHMETICOPERATOR
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

term operator operand = do
  opr <- operator
  rhs <- operand
  return (opr, rhs)
