definition module Lamba.Tokenizer

import StdEnv
import Data.Error
import Lamba.Language.Token

:: TokenizerErrorType = IllegalCharacterDigit Char
	| IllegalCharacterIdentifier Char
	| UnknownToken Char
	| CharNotTerminated
	| StringNotTerminated

:: TokenizerError = TokenizerError TokenLocation TokenizerErrorType

:: TokenizerState = { stream :: String
					, index :: Int
					, line :: Int
					, column :: Int
					, errors :: [TokenizerError]
					, tokens :: [(TokenLocation, Token)]}

instance == TokenizerError
instance toString TokenizerError

tokenize :: String -> MaybeError TokenizerError [(TokenLocation, Token)]
