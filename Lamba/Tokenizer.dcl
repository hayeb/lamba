definition module Lamba.Tokenizer

import StdEnv
import Data.Either
import Lamba.Language.Token

:: TokenizerErrorType = IllegalCharacterDigit Char
	| IllegalCharacterIdentifier Char
	| UnknownToken Char

:: TokenizerLocation :== (Int, Int)
:: TokenizerError = TokenizerError TokenizerLocation TokenizerErrorType

:: TokenizerState = { stream :: String
					, index :: Int
					, line :: Int
					, column :: Int
					, errors :: [TokenizerError]
					, tokens :: [(TokenizerLocation, Token)]}

instance == TokenizerError
instance toString TokenizerError
instance toString (TokenizerLocation, Token)

tokenize :: String -> Either TokenizerError [(TokenizerLocation, Token)]
