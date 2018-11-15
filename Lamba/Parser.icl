implementation module Lamba.Parser

import Data.Either
import Lamba.Language.Token
import Lamba.Language.AST

parse :: [((Int, Int), Token)] -> Either ParseError AST
parse _ = Right (AST [])
