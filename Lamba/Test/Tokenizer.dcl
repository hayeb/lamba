definition module Lamba.Test.Tokenizer

import Data.Either

import Lamba.Tokenizer

tokenizerTests :: [(String, String, Either TokenizerError [(TokenizerLocation, Token)])]