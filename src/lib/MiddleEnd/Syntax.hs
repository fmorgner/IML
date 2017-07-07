module Syntax where

import Data.Maybe

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

data IMLRelationalOperator
  = LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  | Equal
  | NotEqual
  | Literal IMLLiteralExpression

data IMLCommand
  = Assignment IMLAssignment
  | Conditional IMLConditional
  | Loop IMLLoop
  | CompoundCommand [IMLCommand]
  | NoOp

data IMLAssignment
  = SimpleAssignment IMLIdentifier IMLExpression
  | MultiAssignment [IMLIdentifier] [IMLExpression]

data IMLConditional
  = If IMLBooleanExpression IMLCommand (Maybe IMLCommand)

data IMLLoop
  = While IMLBooleanExpression IMLCommand
  | For IMLIdentifier IMLArithmeticExpression IMLArithmeticExpression IMLCommand

newtype IMLIdentifier = Identifier String

data IMLExpression
  = BooleanExpression IMLBooleanExpression
  | ArithmeticExpression IMLArithmeticExpression
  | LiteralExpression IMLLiteralExpression

data IMLBooleanExpression
  = Negation IMLBooleanExpression
  | Comparison IMLBooleanExpression IMLBooleanOperator IMLBooleanExpression
  | Relation IMLArithmeticExpression IMLRelationalOperator IMLArithmeticExpression
  | BooleanLiteralExpression IMLLiteralExpression

data IMLArithmeticExpression
  = Binary IMLArithmeticExpression IMLArithmeticOperator IMLArithmeticExpression
  | NumericLiteralExpression IMLLiteralExpression

data IMLLiteralExpression
  = NumericLiteral Integer
  | BooleanLiteral Bool
  | StringLitreal String
