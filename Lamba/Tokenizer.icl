implementation module Lamba.Tokenizer

import Data.Either, Text, StdChar, Data.Func
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

instance toString (TokenizerLocation, Token)
where
	toString ((line, col), token) = "[" + toString line + ":" + toString col + "] " + toString token

putState :: Int Int ((Int, Int), Token) TokenizerState -> TokenizerState
putState lineInc colInc token state
= putStateSep colInc lineInc colInc token state

putStateSep :: Int Int Int ((Int, Int), Token) TokenizerState -> TokenizerState
putStateSep indexInc lineInc colInc token state=:{index, line,column,tokens}
| not (trace_tn ("putStateSep " + toString token) ) = undef
= {state & index = index + indexInc
  , line = line + lineInc
  , column = column + colInc
  , tokens = [token : tokens]}

advState :: Int Int TokenizerState -> TokenizerState
advState lineInc colInc state
= advStateSep colInc lineInc colInc state

advStateSep :: Int Int Int TokenizerState -> TokenizerState
advStateSep indexInc lineInc colInc state=:{index, line, column}
| not (trace_tn ("advStateSep [" + toString lineInc + "," + toString colInc + "]")) = undef
= { state & index = index + indexInc
  , line = line + lineInc
  , column = column + colInc}

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
		= tokenize` $ putState 0 (length + 2) token st 
	| isSymbol char = tokenize` $ putState 0 1 ((line, column), Symbol char) st
	| char == ' ' = tokenize` $ advState 0 1 st
	| char == '\t' = tokenize` $ advStateSep 1 0 4 st
	| char == '\n' = tokenize` $ putStateSep 1 1 (column * -1 + 1) ((line, column), Symbol '\n') st
	| isDigit char = case tokenizeDigit stream index of
		Left e = Left (TokenizerError (line, column) e)
		Right length
		# token = ((line, column), Number (toInt (subString index length stream)))
		= tokenize` $ putState 0 length token st
	| isAlpha char || char == '_' = case tokenizeIdentifier stream index of
		Left e = Left (TokenizerError (line, column) e)
		Right length
		# token = ((line, column), Identifier (subString index length stream))
		= tokenize` $ putState 0 length token st
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
