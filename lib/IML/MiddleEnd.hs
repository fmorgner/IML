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

{-|
Module      : IML.MiddleEnd
Description : The IML interpreter middle-end
Copyright   : (c) Felix Morgner, 2017
License     : 3-clause BSD
Maintainer  : felis.morgner@gmail.com

This module contains functions and types of the IML interpreter middle-end. The
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
