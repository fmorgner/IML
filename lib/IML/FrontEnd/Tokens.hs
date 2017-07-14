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
  | NOT
  deriving (Eq, Show)

data Attribute
  = NumericValue Integer
  | BooleanValue Bool
  | StringValue String
  | Name String
  | AdditiveOperator IMLAdditiveOperator
  | MultiplicativeOperator IMLMultiplicativeOperator
  | BooleanOperator IMLBooleanOperator
  | RelationalOperator IMLRelationalOperator
  deriving (Eq, Show)
