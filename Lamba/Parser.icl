implementation module Lamba.Parser

import Data.Either
import Lamba.Language.Token
import Lamba.Language.AST

instance toString ParseError
where
	toString (ParseError error) = "ParseError: " +++ error

parse :: [((Int, Int), Token)] -> Either ParseError AST
parse _ = Right (AST [])
