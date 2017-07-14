{-
 - Copyright (c) 2017, Felix Morgner
 -
 - All rights reserved.
 -
 - Redistribution and use in source and binary forms, with or without
 - modification, are permitted provided that the following conditions are met:
 -
 -     * Redistributions of source code must retain the above copyright
 -       notice, this list of conditions and the following disclaimer.
 -
 -     * Redistributions in binary form must reproduce the above
 -       copyright notice, this list of conditions and the following
 -       disclaimer in the documentation and/or other materials provided
 -       with the distribution.
 -
 -     * Neither the name of Felix Morgner nor the names of other
 -       contributors may be used to endorse or promote products derived
 -       from this software without specific prior written permission.
 -
 - THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 - "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 - LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 - A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 - OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 - SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 - LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 - DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 - THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 - (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 - OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 -}

module IML.MiddleEnd.Syntax where

import Data.Maybe

newtype IMLIdentifier = Identifier String
  deriving (Eq, Show)

data IMLAdditiveOperator
  = Plus
  | Minus
  deriving (Eq, Show)

data IMLMultiplicativeOperator
  = Times
  | DivideBy
  | Modulo
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
  | For IMLIdentifier IMLAdditiveExpression IMLAdditiveExpression IMLCommand
  deriving (Eq, Show)

data IMLExpression
  = BooleanExpression IMLBooleanExpression
  | AdditiveExpression IMLAdditiveExpression
  | LiteralExpression IMLLiteralExpression
  deriving (Eq, Show)

data IMLBooleanOperand
  = BooleanLiteralOperand IMLLiteralExpression
  | BooleanIdentifierOperand IMLIdentifier
  | BooleanExpressionOperand IMLBooleanExpression
  deriving (Eq, Show)

data IMLBinaryBooleanOperator
  = And
  | Or
  deriving (Eq, Show)

data IMLUnaryBooleanOperator
  = Not
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

data IMLBooleanExpression
  = Combination IMLBooleanOperand [(IMLBinaryBooleanOperator, IMLBooleanOperand)]
  | Relation IMLAdditiveExpression IMLRelationalOperator IMLAdditiveExpression
  | BooleanUnary IMLUnaryBooleanOperator IMLBooleanOperand
  deriving (Eq, Show)

data IMLArithmeticOperand
  = NumericOperand IMLLiteralExpression
  | IdentifierOperand IMLIdentifier
  deriving (Eq, Show)

data IMLAdditiveExpression
  = Additive IMLMultiplicativeExpression [(IMLAdditiveOperator, IMLMultiplicativeExpression)]
  deriving (Eq, Show)

data IMLMultiplicativeExpression
  = Multiplicative IMLArithmeticOperand [(IMLMultiplicativeOperator, IMLArithmeticOperand)]
  deriving (Eq, Show)

data IMLLiteralExpression
  = NumericLiteral Integer
  | BooleanLiteral Bool
  | StringLiteral String
  deriving (Eq, Show)
