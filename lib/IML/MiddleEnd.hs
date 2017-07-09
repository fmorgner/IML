{-|
Module      : IML.MiddleEnd
Description : The IML interpreter middle-end
Copyright   : (c) Felix Morgner, 2017
License     : 3-clause BSD
Maintainer  : felis.morgner@gmail.com

This module contains functions types of the IML interpreter middle-end. The
middle-end is responsible for parsing and generating the AST.
-}
module IML.MiddleEnd (
  module IML.MiddleEnd.Parser,
  module IML.MiddleEnd.ProductionHelpers,
  module IML.MiddleEnd.Productions,
  module IML.MiddleEnd.Syntax) where

import IML.MiddleEnd.Parser
import IML.MiddleEnd.ProductionHelpers
import IML.MiddleEnd.Productions
import IML.MiddleEnd.Syntax
