definition module Lamba.Parser

import Data.Error

import Lamba.Language.Token
import Lamba.Language.AST

:: ParseError = General (Int, Int) String
instance toString ParseError

parse :: [((Int, Int), Token)] -> MaybeError ParseError AST
