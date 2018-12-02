definition module Lamba

import StdEnv, Data.Either, Data.Maybe, Data.Tuple

import Lamba.Tokenizer
import Lamba.Parser
import Lamba.TypeInference
import Lamba.Formatter
import Lamba.Language.Token
import Lamba.Language.AST

instance == (Either a b) | == a & == b
instance == (MaybeError e a) | == e & == a

instance toString (a, b) | toString a & toString b
