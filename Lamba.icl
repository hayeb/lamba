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

instance == (MaybeError e r) | == e & == r
where
	(==) (Error e1) (Error e2) = e1 == e2
	(==) (Error e1) (Ok r2) = False
	(==) (Ok r1) (Error e2) = False
	(==) (Ok r1) (Ok r2) = r1 == r2

instance toString (a, b) | toString a & toString b
where
	toString (a, b) = "(" + toString a + ", " + toString b + ")"
