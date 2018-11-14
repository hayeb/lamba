definition module Lamba

import StdEnv, Data.Either, Data.Maybe, Data.Tuple

import Lamba.Tokenizer
import Lamba.Language.Token

instance toString (TokenizerLocation, Token)
instance == (Either a b) | == a & == b
