definition module Lamba.Tokenizer

import StdEnv
import Data.Either
import Lamba.Language.Token

:: TokenizerErrorType = IllegalCharacterDigit Char
	| IllegalCharacterIdentifier Char
	| UnknownToken Char

:: TokenizerError = TokenizerError TokenLocation TokenizerErrorType

:: TokenizerState = { stream :: String
					, index :: Int
					, line :: Int
					, column :: Int
					, errors :: [TokenizerError]
					, tokens :: [(TokenLocation, Token)]}

instance == TokenizerError
instance toString TokenizerError
instance toString (TokenLocation, Token)

tokenize :: String -> Either TokenizerError [(TokenLocation, Token)]
