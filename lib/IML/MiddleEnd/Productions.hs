{-|
Module      : IML.MiddleEnd.Productions
Description : Functions for the IML production rules
Copyright   : (c) Felix Morgner, 2017
License     : 3-clause BSD
Maintainer  : felis.morgner@gmail.com

This module contains functions representing the production rules for IML.
-}
module IML.MiddleEnd.Productions
   (
   -- * Literals
   literalExpression,
   numericLiteral,
   booleanLiteral,
   stringLiteral,
   -- * Operators
   additiveOperator,
   multiplicativeOperator,
   unaryBooleanOperator,
   binaryBooleanOperator,
   relationalOperator,
   -- * Identifiers
   identifier,
   -- * Expressions
   expression,
   -- ** Arithmetic expressions
   arithmeticOperand,
   additiveExpression,
   multiplicativeExpression,
   -- ** Boolean expressions
   booleanOperand,
   booleanExpression
   ) where

import IML.FrontEnd.Tokens
import IML.MiddleEnd.Parser
import IML.MiddleEnd.Syntax
import IML.MiddleEnd.ProductionHelpers
import Control.Monad
import Control.Applicative

{-------------------
Literals
--------------------}

{-|
The __literal_expression__ production:

@
__literal_expression__ = __numeric_literal__
                   | __boolean_literal__
                   | __string_literal__ ;
@
-}
literalExpression :: Parser IMLLiteralExpression
literalExpression =  numericLiteral
                 <|> booleanLiteral
                 <|> stringLiteral

{-|
The __numeric_literal__ production:

@
__numeric_literal__ = [ - ] , __digit__ , { __digit__ } ;

__digit__           = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 ;
@
-}
numericLiteral :: Parser IMLLiteralExpression
numericLiteral = do
  (Token NUMERIC (Just (NumericValue n))) <- expect NUMERIC
  return (NumericLiteral n)

{-|
The __boolean_literal__ production:

@
__boolean_literal__ = true
                | false ;
@
-}
booleanLiteral :: Parser IMLLiteralExpression
booleanLiteral = do
  (Token BOOLEAN (Just (BooleanValue b))) <- expect BOOLEAN
  return (BooleanLiteral b)

{-|
The __string_literal__ production:

@
__string_literal__ = ' { __digit__ | __letter__ } ' ;

__letter__         = a | b | c | d | e | f | g | h | i | j | k | l | m | n
               | o | p | q | r | s | t | u | v | w | x | y | z | A | B
               | C | D | E | F | G | H | I | J | K | L | M | N | O | P
               | Q | R | S | T | U | V | W | X | Y | Z ;
@
-}
stringLiteral :: Parser IMLLiteralExpression
stringLiteral = do
  (Token STRING (Just (StringValue s))) <- expect STRING
  return (StringLiteral s)

{--------
Operators
---------}

{-|
The __additive_operator__ production:

@
__additive_operator__ = + | - ;
@
-}
additiveOperator =  additiveOp Plus
                <|> additiveOp Minus

{-|
The __additive_operator__ production:

@
__additive_operator__ = + | - ;
@
-}
multiplicativeOperator =  multiplicativeOp Times
                      <|> multiplicativeOp DivideBy
                      <|> multiplicativeOp Modulo

{-|
The __relational_operator__ production:

@
__relational_operator__ = \< \| \<\= \| > | >= | = | /= ;
@
-}
relationalOperator :: Parser IMLRelationalOperator
relationalOperator =  relaOp LessThan
                  <|> relaOp LessThanOrEqual
                  <|> relaOp GreaterThan
                  <|> relaOp GreaterThanOrEqual
                  <|> relaOp Equal
                  <|> relaOp NotEqual

{-|
The __binary_boolean_operator__ production:

@
__binary_boolean_operator__ = ^ | v ;
@
-}
binaryBooleanOperator :: Parser IMLBinaryBooleanOperator
binaryBooleanOperator =  boolOp And
                     <|> boolOp Or

{-|
The __unary_boolean_operator__ production:

@
__unary_boolean_operator__ = ! ;
@
-}
unaryBooleanOperator :: Parser IMLUnaryBooleanOperator
unaryBooleanOperator = expect NOT >> return Not

{----------
Identifiers
-----------}

{-|
The __identifier__ production:

@
__identifier__ = __letter__ , { __letter__ | __digit__ };
@
-}
identifier :: Parser IMLIdentifier
identifier = do
  (Token IDENTIFIER (Just (Name n))) <- expect IDENTIFIER
  return (Identifier n)

{----------
Expressions
-----------}

{-|
The __expression__ production:

@
__expression__ = __boolean_expression__
           | __additive_expression__
           | __literal_expression__ ;
@
-}
expression :: Parser IMLExpression
expression =  BooleanExpression <$> booleanExpression
          <|> AdditiveExpression <$> additiveExpression
          <|> LiteralExpression <$> (booleanLiteral
                                 <|> numericLiteral
                                 <|> stringLiteral)

{---------------------
Expressions.Arithmetic
----------------------}

{-|
The __arithmetic_operand__ production:

@
__arithmetic_operand__ = __numeric_literal__
                   | __identifier__
@
-}
arithmeticOperand :: Parser IMLArithmeticOperand
arithmeticOperand =  NumericOperand <$> numericLiteral
                 <|> IdentifierOperand <$> identifier

{-|
The __additive_expression__ production:

@
__additive_expression__ = __multiplicative_expression__ { __additive_operator__ __multiplicative_expression__ } ;
@
-}
additiveExpression :: Parser IMLAdditiveExpression
additiveExpression = do
  lhs <- multiplicativeExpression
  terms <- some $ term additiveOperator multiplicativeExpression
  return $ Additive lhs terms

{-|
The __multiplicative_expression__ production:

@
__multiplicative_expression__ = __arithmetic_operand__ { __multiplicative_operator__ __arithmetic_operand__ } ;
@
-}
multiplicativeExpression :: Parser IMLMultiplicativeExpression
multiplicativeExpression = do
  lhs <- arithmeticOperand
  terms <- some $ term multiplicativeOperator arithmeticOperand
  return $ Multiplicative lhs terms

{------------------
Expressions.Boolean
-------------------}

{-|
The __boolean_operand__ production:

@
__boolean_operand__ = __boolean_literal__
                | __identifer__
                | "(" __boolean_expression__ ")" ;
@
-}
booleanOperand :: Parser IMLBooleanOperand
booleanOperand =  BooleanLiteralOperand <$> booleanLiteral
              <|> BooleanIdentifierOperand <$> identifier
              <|> BooleanExpressionOperand <$> ((expect LEFTPAREN >> booleanExpression) << expect RIGHTPAREN)
              <|> do
                op  <- unaryBooleanOperator
                rhs <- booleanOperand
                return $ BooleanExpressionOperand $ BooleanUnary op rhs

{-|
The __boolean_expression__ production:

@
__boolean_expression__ = __boolean_operand__ { __binary_boolean_operator__ __boolean_operand__ }
                   | __additive_expression__ __relational_operator__ __additive_expression__
                   | __unary_boolean_operator__ __boolean_operand__ ;
@
-}
booleanExpression :: Parser IMLBooleanExpression
booleanExpression =  do
                    lhs <- additiveExpression
                    op  <- relationalOperator
                    rhs <- additiveExpression
                    return $ Relation lhs op rhs
                 <|> do
                    lhs <- booleanOperand
                    terms <- some $ term binaryBooleanOperator booleanOperand
                    return $ Combination lhs terms
