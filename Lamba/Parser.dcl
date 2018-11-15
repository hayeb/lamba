definition module Lamba.Parser

import Data.Either

import Lamba.Language.Token
import Lamba.Language.AST

:: ParseError = ParseError String

parse :: [((Int, Int), Token)] -> Either ParseError AST
