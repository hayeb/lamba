implementation module Lamba.Parser

from Control.Monad import class Monad(..) 
import Control.Applicative
import Data.Either, Data.Error, Data.Functor

import Lamba.Language.Token
import Lamba.Language.AST

import StdMisc, StdDebug, Text

parseDebug = True

debug m
| parseDebug = trace_n m False
= False

db m val
| parseDebug = trace_n m val
= val

:: TokenList :== [(TokenLocation, Token)]
:: Parser a = Parser (TokenList -> [(ParseResult a, TokenList)])
:: ParseResult a = Parsed a
	| Failed ParseError

instance Functor ParseResult
where
	fmap f (Parsed a) = Parsed (f a)
	fmap f (Failed e) = Failed e

instance Functor Parser
where
	fmap f (Parser a) = Parser \inp.  
		[(fmap f res, tokens) \\ (res, tokens) <- a inp]

instance Applicative Parser
where
	pure a = Parser \inp. [(Parsed a, inp)]
	(<*>) (Parser fp) (Parser ap) = Parser \inp. 
		[(apply f a, inp``) \\ (a, inp`) <- ap inp, (f, inp``) <- fp inp`]
	where
		apply (Parsed f) a = fmap f a
		apply (Failed e) _ = Failed e

instance Monad Parser
where
	bind (Parser pa) pfa = Parser \inp. 
		[(b, input``) \\ (a, input`) <- pa inp, (b, input``) <- bind` pfa a input`]
	where
		bind` pfa (Parsed r) input = case pfa r of
			(Parser f) = f input
		bind` pfa (Failed e) input = [(Failed e, input)]
	
instance toString ParseError
where
	toString (General error) = "ParseError: " +++ error

zero = Parser \inp. []

// Returns a token from the stream if available.
item = Parser \inp. case inp of
	[] = []
	[t:ts] = [(Parsed t, ts)]

// Choice combinator: First tries the left parser. When if fails, tries the right.
(<<|>) infixl 0 :: (Parser a) (Parser a) -> Parser a
(<<|>) (Parser l) (Parser r) = Parser \inp. case l inp of
	[] = r inp
	ls = ls

err :: String -> Parser a
err msg = Parser \inp. [(Failed (General msg), inp)]
		
// Retrieves a token when it satisfies the predicate
sat :: (Token -> Bool) -> Parser (TokenLocation, Token)
sat p = item
	>>= \(loc, token). if (p token) (pure (loc, token)) zero

// zero or more items
many :: (Parser a) -> Parser [a]
many p = (p
	>>= \i. many p
	>>= \is. return [i : is])
	<<|> return []

// one or more items
some :: (Parser a) -> Parser [a]
some p = p
	>>= \i. many p
	>>= \is. return [i : is] 

// Skip an optional newline
optionalNewline :: Parser [Char]
optionalNewline = many (pSymbol '\n')
	
// Parse an identifier
pIdentifier :: Parser String
pIdentifier = sat (\t. case t of (Identifier s) = True; _ = False)
	>>= \(_, (Identifier s)). pure s

pSpecificIdentifier :: String -> Parser String
pSpecificIdentifier name = pIdentifier
	>>= \s. if (s == name) (pure s) zero

// Parse a number
pNumber :: Parser Int
pNumber = sat (\t. case t of (Number _) = True; _ = False)
	>>= \(_, (Number i)). pure i

// Parse a string literal
pString :: Parser String
pString = sat (\t. case t of (StringLiteral _) = True; _ = False)
	>>= \(_, (StringLiteral s)). pure s

pChar :: Parser Char
pChar = sat (\t. case t of (CharacterLiteral c) = True;  _ = False)
	>>= \(_, (CharacterLiteral c)). pure c

pBool :: Parser Bool
pBool = sat (\t. case t of (Identifier bool) = bool == "True" || bool == "False"; _ = False)
	>>= \(_, (Identifier bool)). pure (if (bool == "True") True False)

pSymbol :: Char -> Parser Char
pSymbol c = sat (\t. case t of (Symbol s) = s == c; _ = False)
	>>= \(_, (Symbol c)). pure c

pSymbols :: String -> Parser String
pSymbols cs = symbols` (fromString cs)
	>>= \_. pure cs
where
	symbols` [] = pure []
	symbols` [c:cs] = pSymbol c
		>>= \c. symbols` cs 

pType :: Parser Type
pType = pSimpleType 
	>>= \lt. ((pSymbols "->" 
			>>| optionalNewline
			>>| pType
			>>= \rest. db "Function type" (pure (TFunc lt rest)))
		<<|> db "Non-function type" (pure lt))
where
	pSimpleType = pSpecificIdentifier "Bool" >>| pure TBool
		<<|> pSpecificIdentifier "Int" >>| pure TInt
		<<|> pSpecificIdentifier "Char" >>| pure TChar
		<<|> pSpecificIdentifier "String" >>| pure TString
		<<|> pSpecificIdentifier "Void" >>| pure TVoid

pExpr :: Parser Expr
pExpr 
| debug "parsing expression" = undef
= pExpr1
where
	pExpr1 :: Parser Expr
	pExpr1 = pExpr2 
		>>= \l. ((pSymbols "||" >>= \_. pExpr2 >>= \r. pure (OrExpr l r))
			<<|> (pSymbols "&&" >>= \_.  pExpr2 >>= \r. pure (AndExpr l r)))
			<<|> return l

	pExpr2 :: Parser Expr
	pExpr2 = pExpr3 
		>>= \l. ((pSymbols "==" >>= \_. pExpr3 >>= \r. pure (EqExpr l r))
			<<|> (pSymbols "<=" >>= \_.  pExpr3 >>= \r. pure (LeqExpr l r))
			<<|> (pSymbols ">=" >>= \_.  pExpr3 >>= \r. pure (GeqExpr l r))
			<<|> (pSymbols "!=" >>= \_.  pExpr3 >>= \r. pure (NeqExpr l r))
			<<|> (pSymbol '<' >>= \_.  pExpr3 >>= \r. pure (LesserExpr l r))
			<<|> (pSymbol '>' >>= \_.  pExpr3 >>= \r. pure (GreaterExpr l r))
			<<|> return l
			)
	
	pExpr3 :: Parser Expr
	pExpr3 = pExpr4
		>>= \l. ((pSymbol '+' >>| pExpr4 >>= \r. pure (PlusExpr l r))
			<<|> (pSymbol '-' >>| pExpr4 >>= \r. pure (MinusExpr l r))
			<<|> return l
			)

	pExpr4 :: Parser Expr
	pExpr4 = pExpr5
		>>= \l. ((pSymbol '*' >>| pExpr5 >>= \r. pure (TimesExpr l r))
			<<|> (pSymbol '/' >>| pExpr5 >>= \r. pure (DivideExpr l r))
			<<|> (pSymbol '%' >>| pExpr5 >>= \r. pure (ModuloExpr l r))
			<<|> return l)
	
	pExpr5 :: Parser Expr
	pExpr5 = (pNumber >>= \n. return (NumberExpr n))
		<<|> (pString >>= \s. return (StringExpr s))
		<<|> (pChar >>= \c. return (CharExpr c))
		<<|> (pSpecificIdentifier "True" >>| return (BoolExpr True))
		<<|> (pSpecificIdentifier "False" >>| return (BoolExpr False))
		<<|> (pSymbol '(' 
			>>| pExpr1 
			>>= \sub. pSymbol ')' 
			>>| return (Nested sub))
		<<|> pTuple
		<<|> (pIdentifier 
			>>= \fName. many pExpr1 
			>>= \args. return (FuncExpr fName args))
	
	pTuple
	= pSymbol '('
		>>| db "parsed (" pExpr1
		>>= \e1. pTuple`
		>>= \rest. pSymbol ')'
		>>| return (TupleExpr [e1:rest])
	where
		pTuple` = some (pSymbol ',' >>| pExpr1)

pFGuard :: Parser FGuard
pFGuard 
| debug "Parsing function guard" = undef
= (pSymbols "\n|" 
		>>| pExpr 
		>>= \ge. db  "Parsed guard left" (pSymbol '=') 
		>>| pExpr 
		>>= \re. db "Parsed guard right" (pure (Guarded ge re)))
	<<|> (optionalNewline
		>>| pSymbols "=" 
		>>| pExpr 
		>>= \e. pure (NonGuarded e))

pMatch :: Parser Match
pMatch 
| debug "Parsing function match" = undef
= (pIdentifier >>= \id. return (MVar id))
	<<|> (pString >>= \str. return (MString str))
	<<|> (pChar >>= \char. return (MChar char))
	<<|> (pBool >>= \bool. return (MBool bool))
	<<|> (pNumber >>= \num. return (MNum num))
	<<|> (pSymbol '(' >>| tupleEls >>= \els. pSymbol ')' >>| return (MTuple els))
where
	tupleEls = pMatch 
		>>= \e. pSymbol ','
		>>| tupleEls`
		>>= \es. return [e : es]
	where
		tupleEls` = (pMatch
			>>= \e. pSymbol ','
			>>| tupleEls`
			>>= \es. return [e : es])
			<<|> return []

pFBody :: String -> Parser FBody
pFBody fname
| debug "Parsing function body" = undef
= pSpecificIdentifier fname
	>>| db "Correct function name" (many pMatch)
	>>= \arguments. db ("Parsed arguments [" + join " " (map toString arguments) + "]") (some pFGuard)
	>>= \guards. pure (FBody arguments guards)

pFDecl :: Parser FDecl
pFDecl 
| debug "Parsing function declaration" = undef
= pIdentifier 
	>>= \fName. db ("Got function name: " + fName) (pSymbols "::")
	>>| db "Parsing type" pType
	>>= \fType. db ("Parsed type " + toString fType) (some ( pSymbol '\n' >>| pFBody fName))
	>>= \fBody. pure (FDecl fName fType fBody)

pAst :: Parser AST
pAst = some (optionalNewline >>| pFDecl)
	>>= \fdecls. pure (AST fdecls)

parse :: [((Int, Int), Token)] -> Either ParseError AST
parse inp = case pAst of
	(Parser f) = case f inp of
		[] = Left (General "Parsing failed")
		[(Parsed ast, _):_] = Right ast
		[(Failed e,_):_] = Left e

