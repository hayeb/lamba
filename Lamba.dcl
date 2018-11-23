definition module Lamba

import StdEnv, Data.Either, Data.Maybe, Data.Tuple

import Lamba.Tokenizer
import Lamba.Parser
import Lamba.TypeChecker
import Lamba.Formatter
import Lamba.Language.Token
import Lamba.Language.AST

instance == (Either a b) | == a & == b
