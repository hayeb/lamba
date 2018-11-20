definition module Lamba.Parser

import Data.Either

import Lamba.Language.Token
import Lamba.Language.AST

:: ParseError = General (Int, Int) String
instance toString ParseError

parse :: [((Int, Int), Token)] -> Either ParseError AST
