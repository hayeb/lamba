implementation module Lamba.Language.Token

import StdEnv, StdMisc, Text

instance == Token
where
	(==) (Identifier id1) (Identifier id2) = id1 == id2
	(==) (Number n1) (Number n2) = n1 == n2
	(==) (StringLiteral l1) (StringLiteral l2) = l1 == l2
	(==) (CharacterLiteral c1) (CharacterLiteral c2) = c1 == c2
	(==) (Symbol c1) (Symbol c2) = c1 == c2
	(==) _ _ = False

instance toString Token
where
	toString (Identifier id) = "Identifier " + id
	toString (Number num) = "Number " + toString num
	toString (StringLiteral lit) = "StringLiteral \"" +++ lit +++ "\""
	toString (CharacterLiteral c) = "CharacterLiteral " + toString c
	toString (Symbol c) = "Symbol " + toString c
