module IML.MiddleEnd.Syntax where

import Data.Maybe

newtype IMLIdentifier = Identifier String
  deriving (Eq, Show)

data IMLArithmeticOperator
  = Times
  | DivideBy
  | Modulo
  | Plus
  | Minus
  deriving (Eq, Show)

data IMLBooleanOperator
  = And
  | Or
  deriving (Eq, Show)

data IMLRelationalOperator
  = LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  | Equal
  | NotEqual
  | Literal IMLLiteralExpression
  deriving (Eq, Show)

data IMLCommand
  = Assignment IMLAssignment
  | Conditional IMLConditional
  | Loop IMLLoop
  | CompoundCommand [IMLCommand]
  | NoOp
  deriving (Eq, Show)

data IMLAssignment
  = SimpleAssignment IMLIdentifier IMLExpression
  | MultiAssignment [IMLIdentifier] [IMLExpression]
  deriving (Eq, Show)

data IMLConditional
  = If IMLBooleanExpression IMLCommand (Maybe IMLCommand)
  deriving (Eq, Show)

data IMLLoop
  = While IMLBooleanExpression IMLCommand
  | For IMLIdentifier IMLArithmeticExpression IMLArithmeticExpression IMLCommand
  deriving (Eq, Show)

data IMLExpression
  = BooleanExpression IMLBooleanExpression
  | ArithmeticExpression IMLArithmeticExpression
  | LiteralExpression IMLLiteralExpression
  deriving (Eq, Show)

data IMLBooleanExpression
  = Negation IMLBooleanExpression
  | Comparison IMLBooleanExpression IMLBooleanOperator IMLBooleanExpression
  | Relation IMLArithmeticExpression IMLRelationalOperator IMLArithmeticExpression
  | BooleanLiteralExpression IMLLiteralExpression
  | BooleanIdentifierExpression IMLIdentifier
  deriving (Eq, Show)

data IMLArithmeticExpression
  = Binary IMLArithmeticExpression IMLArithmeticOperator IMLArithmeticExpression
  | NumericLiteralExpression IMLLiteralExpression
  | ArithmeticIdentifierExpression IMLIdentifier
  deriving (Eq, Show)

data IMLLiteralExpression
  = NumericLiteral Integer
  | BooleanLiteral Bool
  | StringLiteral String
  deriving (Eq, Show)
