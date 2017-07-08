module IML.FrontEnd.Lexer
  ( tokenize
  ) where

import Data.Char
import IML.FrontEnd.Tokens
import IML.MiddleEnd.Syntax

tokenize :: String -> [Token]
tokenize input = tokenize' (input, [])

-- Internal implementation below

letters = ['a' .. 'z'] ++ ['A' .. 'Z']
digits  = ['0' .. '9']

keywords "true"  = Token BOOLEAN (Just $ BooleanValue True)
keywords "false" = Token BOOLEAN (Just $ BooleanValue False)
keywords "if"    = Token IF Nothing
keywords "else"  = Token ELSE Nothing
keywords "for"   = Token FOR Nothing
keywords "while" = Token WHILE Nothing
keywords "noop"  = Token NOOP Nothing
keywords x       = Token IDENTIFIER (Just $ Name x)

arithOp op = Token ARITHMETICOPERATOR $ Just $ ArithmeticOperator op
relaOp  op = Token RELATIONALOPERATOR $ Just $ RelationalOperator op

tokenize' :: (String, [Token]) -> [Token]
tokenize' ('+' :             rest, toks) = tokenize' (rest, arithOp Plus : toks)
tokenize' ('-' : ' ' :       rest, toks) = tokenize' (rest, arithOp Minus : toks)
tokenize' ('*' :             rest, toks) = tokenize' (rest, arithOp Times : toks)
tokenize' ('/' : ' ' :       rest, toks) = tokenize' (rest, arithOp DivideBy : toks)
tokenize' ('%' :             rest, toks) = tokenize' (rest, arithOp Modulo : toks)
tokenize' ('<' : '-' :       rest, toks) = tokenize' (rest, Token BECOMES Nothing : toks)
tokenize' ('<' : '=' :       rest, toks) = tokenize' (rest, relaOp LessThanOrEqual : toks)
tokenize' ('<' :             rest, toks) = tokenize' (rest, relaOp LessThan : toks)
tokenize' ('>' : '=' :       rest, toks) = tokenize' (rest, relaOp GreaterThanOrEqual : toks)
tokenize' ('>' :             rest, toks) = tokenize' (rest, relaOp GreaterThan : toks)
tokenize' ('=' :             rest, toks) = tokenize' (rest, relaOp Equal : toks)
tokenize' ('/' : '=' :       rest, toks) = tokenize' (rest, relaOp NotEqual : toks)
tokenize' ('^' :             rest, toks) = tokenize' (rest, Token BOOLEANOPERATOR (Just (BooleanOperator And)) : toks)
tokenize' ('v' : ' ' :       rest, toks) = tokenize' (rest, Token BOOLEANOPERATOR (Just (BooleanOperator Or)) : toks)
tokenize' ('(' :             rest, toks) = tokenize' (rest, Token LEFTPAREN Nothing : toks)
tokenize' (')' :             rest, toks) = tokenize' (rest, Token RIGHTPAREN Nothing : toks)
tokenize' ('{' :             rest, toks) = tokenize' (rest, Token LEFTCURLY Nothing : toks)
tokenize' ('}' :             rest, toks) = tokenize' (rest, Token RIGHTCURLY Nothing : toks)
tokenize' (',' :             rest, toks) = tokenize' (rest, Token COMMA Nothing : toks)
tokenize' ('.' : '.' : '.' : rest, toks) = tokenize' (rest, Token TO Nothing : toks)
tokenize' ('\'':             rest, toks) = tokenize' $ tokenizeString (rest, toks)
tokenize' (x   :             rest, toks)
  | isSpace x        = tokenize' (rest, toks)
  | x `elem` digits  = tokenize' $ tokenizeNumeric (x : rest, toks)
  | x `elem` letters = tokenize' $ tokenizeAlpha (x : rest, toks)
  | otherwise        = error ("Unexpected Token '" ++ [x] ++ "'")
tokenize' ([], toks) = reverse toks

tokenizeNumeric :: (String, [Token]) -> (String, [Token])
tokenizeNumeric (x : xs, toks) = numeric (xs, digitToInt x, toks)
  where numeric ([], num, toks) = ([], Token NUMERIC (Just $ ArithmeticValue $ toInteger num) : toks)
        numeric (x : xs, num, toks)
          | isDigit x = numeric (xs, num * 10 + digitToInt x, toks)
          | otherwise = (xs, Token NUMERIC (Just $ ArithmeticValue $ toInteger num) : toks)

tokenizeAlpha :: (String, [Token]) -> (String, [Token])
tokenizeAlpha (x : xs, toks) = alpha (xs, [x], toks)
  where alpha ([],     buf, toks) = ([], keywords (reverse buf) : toks)
        alpha (x : xs, buf, toks)
          | x `elem` letters || x `elem` digits = alpha (xs, x : buf, toks)
          | otherwise = (x:xs, keywords (reverse buf) : toks)

tokenizeString :: (String, [Token]) -> (String, [Token])
tokenizeString ([], _)        = error "Unterminated string literal"
tokenizeString (x : xs, toks) = string (xs, [x], toks)
  where string ([],                 _, _)    = error "Unterminated string literal"
        string ('\'' :        xs, buf, toks) = (xs, Token STRING (Just $ StringValue $ reverse buf) : toks)
        string ('\\' : '\'' : xs, buf, toks) = string (xs, '\'' : buf, toks)
        string (x :           xs, buf, toks) = string (xs, x : buf, toks)
