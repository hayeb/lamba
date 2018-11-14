implementation module Lamba.Tokenizer

import Data.Either, Text, StdChar
import Lamba.Language.Token

import StdMisc, StdDebug

instance == TokenizerError
where
	(==) (TokenizerError l1 t1) (TokenizerError l2 t2) = l1 == l2  && t1 == t2

instance == TokenizerErrorType
where
	(==) (IllegalCharacterDigit t1) (IllegalCharacterDigit t2) = t1 == t2
	(==) (IllegalCharacterIdentifier t1) (IllegalCharacterIdentifier t2) = t1 == t2
	(==) (UnknownToken t1) (UnknownToken t2) = t1 == t2
	(==) _  _ = False

instance toString TokenizerError
where
	toString (TokenizerError (l, c) type) = "[" +toString l + ":" + toString c + "]: " +toString type

instance toString TokenizerErrorType
where
	toString (IllegalCharacterDigit c) = "Illegal character in digit: \'" + toString c + "\'"
	toString (IllegalCharacterIdentifier c) = "Illegal character in identifier: \'" + toString c + "\'"
	toString (UnknownToken c) = "Unknown token: \'" + toString c + "\'"

tokenize :: String -> Either TokenizerError [(TokenizerLocation, Token)]
tokenize s = tokenize` (newState s)
where
	newState string = {stream = string, index = 0, line = 1, column = 1, errors = [], tokens = []}

	tokenize` st=:{stream, index, line, column, errors, tokens}
	| index == size stream = Right (reverse tokens)
	# char = stream.[index]
	| isStringStart char = case tokenizeStringLiteral stream (inc index) of
		Left e = Left (TokenizerError (line, column) e)
		Right length
		# token = ((line, column), StringLiteral (subString (inc index) length stream))
		= tokenize` {st & index = index + length + 2, column = column + length, tokens = [token : tokens]}
	| isSymbol char = tokenize` {st & index = index + 1, column = column + 1, tokens = [((line, column), Symbol char) : tokens]}
	| char == ' ' = tokenize` {st & index = index + 1, column = column + 1}
	| char == '\t' = tokenize` {st & index = index + 1, column = column + 4}
	| char == '\n' = tokenize` {st & index = index + 1, column = 1, line = line + 1}
	| isDigit char = case tokenizeDigit stream index of
		Left e = Left (TokenizerError (line, column) e)
		Right length
		# token = ((line, column), Number (toInt (subString index length stream)))
		= tokenize` {st & index = index + length, column = column + length, tokens = [token : tokens]}
	| isAlpha char || char == '_' = case tokenizeIdentifier stream index of
		Left e = Left (TokenizerError (line, column) e)
		Right length
		# token = ((line, column), Identifier (subString index length stream))
		= tokenize` {st & index = index + length, column = column + length, tokens = [token : tokens]}
	where
		tokenizeIdentifier stream index
		| index == size stream = Right 0
		# char = stream.[index]
		| char == ' ' || char == '\t' || char == '\n' = Right 0
		| char == '_' || isAlphanum char = case tokenizeIdentifier stream (inc index) of
			Left e = Left e
			Right l = Right (inc l)
		= Left (IllegalCharacterIdentifier char)

		tokenizeDigit stream index
		| index == size stream = Right 0
		# char = stream.[index]
		| char == ' ' || char == '\t' || char == '\n' = Right 0
		| isDigit char = case tokenizeDigit stream (inc index) of
			Left e = Left e
			Right l = Right (inc l)
		| isAlpha char = Left (IllegalCharacterDigit char)
		= Right 0

		tokenizeStringLiteral stream index
		| index == size stream = Right 0
		# char = stream.[index]
		| isStringStart char = Right 0
		= case tokenizeStringLiteral stream (inc index) of
			Left e = Left e
			Right l = Right (inc l)

	isSymbol c = (c >= '!' && c <= '/' || c >= ':' && c <= '?' || c >= '[' && c <= '`' || c >= '{' && c <= '~') && not (c == '_')

	isStringStart '"' = True
	isStringStart _ = False
