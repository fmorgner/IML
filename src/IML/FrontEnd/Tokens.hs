module IML.FrontEnd.Tokens where

import IML.MiddleEnd.Syntax
import Data.Maybe

data Token = Token {getTerminal  :: Terminal,
                    getAttribute :: Maybe Attribute}

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
  | BINARARITHMETICOPERATOR
  | BINARYBOOLEANOPERATOR
  | RELATIONALOPERATOR
  deriving (Eq, Show)

data Attribute
  = ArithmeticValue Integer
  | BooleanValue Bool
  | Name String
  | ArithmeticOperator {getOperator :: IMLArithmeticOperator}
  | BooleanOperator {getOperator :: IMLBooleanOperator}
  | RelationalOperator {getOperator :: IMLRelationalOperator}
  deriving (Eq, Show)
