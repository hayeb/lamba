implementation module Lamba.Tokenizer

import  Data.Error, Text, StdChar, Data.Func
import Lamba.Language.Token

import StdMisc, StdDebug

dbTokenizer = False

debug msg
| dbTokenizer = trace_n msg False
= False

instance == TokenizerError
where
	(==) (TokenizerError l1 t1) (TokenizerError l2 t2) = l1 == l2  && t1 == t2

instance == TokenizerErrorType
where
	(==) (IllegalCharacterDigit t1) (IllegalCharacterDigit t2) = t1 == t2
	(==) (IllegalCharacterIdentifier t1) (IllegalCharacterIdentifier t2) = t1 == t2
	(==) (UnknownToken t1) (UnknownToken t2) = t1 == t2
	(==) (CharNotTerminated) (CharNotTerminated) = True
	(==) (StringNotTerminated) (StringNotTerminated) = True
	(==) _  _ = False

instance toString TokenizerError
where
	toString (TokenizerError (l, c) type) = "[" +toString l + ":" + toString c + "]: " +toString type

instance toString TokenizerErrorType
where
	toString (IllegalCharacterDigit c) = "Illegal character in digit: \'" + toString c + "\'"
	toString (IllegalCharacterIdentifier c) = "Illegal character in identifier: \'" + toString c + "\'"
	toString (UnknownToken c) = "Unknown token: \'" + toString c + "\'"
	toString CharNotTerminated = "Character literal not terminated; Are you missing a \'?"
	toString StringNotTerminated = "String literal not terminated; Are you missing a \"?"

instance toString (TokenLocation, Token)
where
	toString ((line, col), token) = "[" + toString line + ":" + toString col + "] " + toString token

putState :: Int Int ((Int, Int), Token) TokenizerState -> TokenizerState
putState lineInc colInc token state
= putStateSep colInc lineInc colInc token state

putStateSep :: Int Int Int ((Int, Int), Token) TokenizerState -> TokenizerState
putStateSep indexInc lineInc colInc token state=:{index, line,column,tokens}
| debug ("putStateSep " + toString token) = undef
= {state & index = index + indexInc
  , line = line + lineInc
  , column = column + colInc
  , tokens = [token : tokens]}

advState :: Int Int TokenizerState -> TokenizerState
advState lineInc colInc state
= advStateSep colInc lineInc colInc state

advStateSep :: Int Int Int TokenizerState -> TokenizerState
advStateSep indexInc lineInc colInc state=:{index, line, column}
| debug ("advStateSep [" + toString lineInc + "," + toString colInc + "]") = undef
= { state & index = index + indexInc
  , line = line + lineInc
  , column = column + colInc}

tokenize :: String -> MaybeError TokenizerError [(TokenLocation, Token)]
tokenize s = tokenize` (newState s)
where
	newState string = {stream = string, index = 0, line = 1, column = 1, errors = [], tokens = []}

	tokenize` st=:{stream, index, line, column, errors, tokens}
	| index == size stream = Ok (reverse tokens)
	# char = stream.[index]
	| isCommentStart char stream index = let commentLength = tokenizeComment stream index in
		tokenize` $ advStateSep commentLength 1 (column * -1 + 1) st
	| isStringStart char = case tokenizeStringLiteral stream (inc index) of
		Error e = Error (TokenizerError (line, column) e)
		Ok length
		# token = ((line, column), StringLiteral (subString (inc index) length stream))
		= tokenize` $ putState 0 (length + 2) token st 
	| isCharStart char = case tokenizeCharLiteral stream (inc index) of
		Error e = Error (TokenizerError (line, column) e)
		Ok c
		# token = ((line, column), CharacterLiteral (stream.[inc index]))
		= tokenize` $ putState 0 3 token st
	| isSymbol char = tokenize` $ putState 0 1 ((line, column), Symbol char) st
	| char == ' ' = tokenize` $ advState 0 1 st
	| char == '\t' = tokenize` $ advStateSep 1 0 4 st
	| char == '\n' = tokenize` $ putStateSep 1 1 (column * -1 + 1) ((line, column), Symbol '\n') st
	| isDigit char = case tokenizeDigit stream index of
		Error e = Error (TokenizerError (line, column) e)
		Ok length
		# token = ((line, column), Number (toInt (subString index length stream)))
		= tokenize` $ putState 0 length token st
	| isAlpha char || char == '_'
		# length = tokenizeIdentifier stream index
		# token = ((line, column), Identifier (subString index length stream))
		= tokenize` $ putState 0 length token st
	where
		tokenizeIdentifier stream index
		| index == size stream = 0
		# char = stream.[index]
		| char == ' ' || char == '\t' || char == '\n' = 0
		| char == '_' || isAlphanum char = inc (tokenizeIdentifier stream (inc index))
		= 0

		tokenizeDigit stream index
		| index == size stream = Ok 0
		# char = stream.[index]
		| char == ' ' || char == '\t' || char == '\n' = Ok 0
		| isDigit char = case tokenizeDigit stream (inc index) of
			Error e = Error e
			Ok l = Ok (inc l)
		| isAlpha char = Error (IllegalCharacterDigit char)
		= Ok 0

		tokenizeStringLiteral stream index
		| index == size stream = Error StringNotTerminated
		# char = stream.[index]
		| isStringStart char = Ok 0
		= case tokenizeStringLiteral stream (inc index) of
			Error e = Error e
			Ok l = Ok (inc l)

		tokenizeCharLiteral stream index
		| index == size stream || index == size stream + 1 = Error CharNotTerminated
		# char = stream.[index]
		| stream.[index + 1] <> '\'' = Error CharNotTerminated
		= Ok char 

		tokenizeComment stream index
		| index == size stream = 0
		# char = stream.[index]
		| char == '\n' = 1
		= inc (tokenizeComment stream (inc index))

		isSymbol c = (c >= '!' && c <= '/' 
				|| c >= ':' && c <= '?' 
				|| c >= '[' && c <= '`' 
				|| c >= '{' && c <= '~') 
			&& not (c == '_')

		isStringStart '"' = True
		isStringStart _ = False

		isCharStart '\'' = True
		isCharStart _ = False

		isCommentStart '/' stream index
		| inc index >= size stream = False
		| stream.[inc index] == '/' = True
		= False
		isCommentStart _ _ _ = False
