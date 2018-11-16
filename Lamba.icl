implementation module Lamba

import StdEnv, Data.Either, Data.Maybe, Data.Tuple, Text

import Lamba.Tokenizer
import Lamba.Language.Token

instance == (Either a b) | == a & == b
where
	(==) (Left _) (Right _) = False
	(==) (Right _) (Left _) = False
	(==) (Left l1) (Left l2) = l1 == l2
	(==) (Right r1) (Right r2) = r1 == r2

