definition module Lamba.Language.Token

import StdEnv

:: Token = Identifier String
	| Number Int
	| StringLiteral String
	| CharacterLiteral Char
	| Symbol Char

:: TokenLocation :== (Int, Int)

instance == Token
instance toString Token
