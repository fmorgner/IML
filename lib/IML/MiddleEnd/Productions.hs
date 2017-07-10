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
  binaryArithmeticOperator,
  binaryBooleanOperator,
  relationalOperator,
  -- * Identifiers
  identifier
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
__literal_expression__ = numeric_literal
                   | boolean_literal
                   | string_literal ;
@
-}
literalExpression :: Parser IMLLiteralExpression
literalExpression =  numericLiteral
                 <|> booleanLiteral
                 <|> stringLiteral

{-|
The __numeric_literal__ production:

@
__numeric_literal__ = { "-" } , digit , { digit } ;
__digit__           = "0" | "1" | "2" | "3" | "4" | "5" | "6"
                | "7" | "8" | "9" ;
@
-}
numericLiteral :: Parser IMLLiteralExpression
numericLiteral = do
  (Token NUMERIC (Just (ArithmeticValue n))) <- expect NUMERIC
  return (NumericLiteral n)

{-|
The __boolean_literal__ production:

@
__boolean_literal__ = "true"
                | "false" ;
@
-}
booleanLiteral :: Parser IMLLiteralExpression
booleanLiteral = do
  (Token BOOLEAN (Just (BooleanValue b))) <- expect BOOLEAN
  return (BooleanLiteral b)

{-|
The __string_literal__ production:

@
__string_literal__ = "'" digit | letter , { digit | letter } "'" ;
__digit__          = "0" | "1" | "2" | "3" | "4" | "5" | "6"
               | "7" | "8" | "9" ;
__letter__         = "a" | "b" | "c" | "d" | "e" | "f" | "g"
               | "h" | "i" | "j" | "k" | "l" | "m" | "n"
               | "o" | "p" | "q" | "r" | "s" | "t" | "u"
               | "v" | "w" | "x" | "y" | "z" | \"A\" | \"B\"
               | \"C\" | \"D\" | \"E\" | \"F\" | \"G\" | \"H\" | \"I\"
               | \"J\" | \"K\" | \"L\" | \"M\" | \"N\" | \"O\" | \"P\"
               | \"Q\" | \"R\" | \"S\" | \"T\" | \"U\" | \"V\" | \"W\"
               | \"X\" | \"Y\" | \"Z\" ;
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
The __binary_arithmetic_operator__ production:

@
__binary_arithmetic_operator__ = "+" | "-" | "*" | "/" | "%" ;
@
-}
binaryArithmeticOperator :: Parser IMLArithmeticOperator
binaryArithmeticOperator =  arithOp Times
                        <|> arithOp DivideBy
                        <|> arithOp Modulo
                        <|> arithOp Plus
                        <|> arithOp Minus

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
binaryBooleanOperator :: Parser IMLBooleanOperator
binaryBooleanOperator =  boolOp And
                     <|> boolOp Or
{----------
Identifiers
-----------}

{-|
The __identifier__ production:

@
__identifier__ = letter , { letter | digit };
@
-}
identifier :: Parser IMLIdentifier
identifier = do
  (Token IDENTIFIER (Just (Name n))) <- expect IDENTIFIER
  return (Identifier n)
