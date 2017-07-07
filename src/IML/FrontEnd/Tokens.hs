module IML.FrontEnd.Tokens where

import IML.MiddleEnd.Syntax
import Data.Maybe

data Token = Token Terminal (Maybe Attribute)
  deriving (Eq, Show)

data Terminal
  = NUMERIC
  | BOOLEAN
  | STRING
  | IDENTIFIER
  | NOOP
  | BECOMES
  | LEFTCURLY
  | RIGHTCURLY
  | LEFTPAREN
  | RIGHTPAREN
  | IF
  | ELSE
  | WHILE
  | FOR
  | TO
  | COMMA
  | ARITHMETICOPERATOR
  | BOOLEANOPERATOR
  | RELATIONALOPERATOR
  deriving (Eq, Show)

data Attribute
  = ArithmeticValue Integer
  | BooleanValue Bool
  | StringValue String
  | Name String
  | ArithmeticOperator IMLArithmeticOperator
  | BooleanOperator IMLBooleanOperator
  | RelationalOperator IMLRelationalOperator
  deriving (Eq, Show)
