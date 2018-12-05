implementation module Lamba.Parser

from Control.Monad import class Monad(..)
import Control.Applicative
import Data.Error, Data.Functor

import Lamba.Language.Token
import Lamba.Language.AST

import StdMisc, StdDebug, Text

parseDebug = False

debug m
| parseDebug = trace_n m False
= False

db m val
| parseDebug = trace_n m val
= val

:: TokenList :== [(TokenLocation, Token)]
:: ParserState = {tokens :: TokenList
	, indent :: Int}

:: Parser a = Parser (ParserState -> [(ParseResult a, ParserState)])
:: ParseResult a = Parsed a
	| Failed ParseError

isParsed (Parsed _) = True
isParsed (Failed _) = False

isFail (Parsed _) = False
isFail (Failed _) = True

instance Functor ParseResult
where
	fmap f (Parsed a) = Parsed (f a)
	fmap f (Failed e) = Failed e

instance Functor Parser
where
	fmap f (Parser a) = Parser \inp.
		[(fmap f res, tokens) \\ (res, tokens) <- a inp]

instance pure Parser
where
	pure a = Parser \inp. [(Parsed a, inp)]

instance <*> Parser
where
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
	toString (General (l, c) error) = "[" + toString l + "," + toString c + "] Error: " +++ error

instance toString (ParseResult a)
where
	toString (Failed err) = "Failed " + toString err
	toString (Parsed _) = "Parsed"

zero = Parser \inp. []

// Returns a token from the stream if available.
item = Parser \st=:{tokens}. case tokens of
	[] = []
	[t:ts] = [(Parsed t, {st & tokens = ts})]

// Returns the location of the next token
loc = Parser \st=:{tokens}. case tokens of
	[] = []
	ts=:[(loc, t):rs] = [(Parsed loc, st)]

locpeek = Parser \st=:{tokens}. case tokens of
	[] = []
	ts=:[(loc, t):rs] = [(Parsed (loc, t), st)]

indent = Parser \st=:{indent}. [(Parsed indent, st)]

incIndent = Parser \st=:{indent}. [(Parsed (inc indent), {st & indent = inc (indent)})]

decIndent = Parser \st=:{indent}. [(Parsed (dec indent), {st & indent = dec (indent)})]

// Choice combinator: First tries the left parser. When if fails, tries the right.
// Discards errors from the left side
(<<|>) infixl 0 :: (Parser a) (Parser a) -> Parser a
(<<|>) (Parser l) (Parser r) = Parser \inp. case l inp of
	[] = r inp
	ls = case [(pa, l) \\ (pa, l) <- ls | isParsed pa] of
		[] = r inp
		ls = ls

(<|>) infixl 0 :: (Parser a) (Parser a) -> Parser a
(<|>) (Parser l) (Parser r) = Parser \inp. case l inp of
	[] = r inp
	ls = ls

err :: ParseError -> Parser a
err err = Parser \inp. [(Failed err, inp)]

strict :: (Parser a) ((TokenLocation, Token) -> ParseError) -> Parser a
strict p onFail = p <<|> (locpeek >>= \tl. pure (onFail tl) >>= err)

// Retrieves a token when it satisfies the predicate
sat :: (Token -> Bool) -> Parser (TokenLocation, Token)
sat p = item
	>>= \(loc, token). if (p token) (pure (loc, token)) zero

// zero or more items
many :: (Parser a) -> Parser [a]
many p
| debug "many items" = undef
=
		(p
		>>= \i. many p
		>>= \is. return [i : is])
	<<|>
		return []

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
	pSimpleType = (pSymbol '('
			>>| pType
			>>= \e. some (pSymbol ',' >>| pType)
			>>= \es. pSymbol ')'
			>>| return (TTuple [e:es]))
		<<|> (pSymbol '['
			>>| pType
			>>= \t. pSymbol ']'
			>>| return (TList t))
		<<|> pSpecificIdentifier "Bool" >>| pure TBool
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
	pExpr1 = loc
		>>= \loc. (pList
		<<|> pExpr2)

	pExpr2 :: Parser Expr
	pExpr2 = loc
		>>= \loc. pExpr3
		>>= \l. ((pSymbols "||" >>= \_. pExpr3 >>= \r. pure (OrExpr loc l r))
			<<|> (pSymbols "&&" >>= \_.  pExpr3 >>= \r. pure (AndExpr loc l r)))
			<<|> return l

	pExpr3 :: Parser Expr
	pExpr3 = loc
		>>= \loc. pExpr4
		>>= \l. ((pSymbols "==" >>= \_. pExpr4 >>= \r. pure (EqExpr loc l r))
			<<|> (pSymbols "<=" >>= \_.  pExpr4 >>= \r. pure (LeqExpr loc l r))
			<<|> (pSymbols ">=" >>= \_.  pExpr4 >>= \r. pure (GeqExpr loc l r))
			<<|> (pSymbols "!=" >>= \_.  pExpr4 >>= \r. pure (NeqExpr loc l r))
			<<|> (pSymbol '<' >>= \_.  pExpr4 >>= \r. pure (LesserExpr loc l r))
			<<|> (pSymbol '>' >>= \_.  pExpr4 >>= \r. pure (GreaterExpr loc l r))
			<<|> return l
			)

	pExpr4 :: Parser Expr
	pExpr4 = loc
		>>= \loc .pExpr5
		>>= \l. ((pSymbol '+' >>| pExpr5 >>= \r. pure (PlusExpr loc l r))
			<<|> (pSymbol '-' >>| pExpr5 >>= \r. pure (MinusExpr loc l r))
			<<|> return l
			)

	pExpr5 :: Parser Expr
	pExpr5 = loc
		>>= \loc. pExpr6
		>>= \l. ((pSymbol '*' >>| pExpr6 >>= \r. pure (TimesExpr loc l r))
			<<|> (pSymbol '/' >>| pExpr6 >>= \r. pure (DivideExpr loc l r))
			<<|> (pSymbol '%' >>| pExpr6 >>= \r. pure (ModuloExpr loc l r))
			<<|> return l)

	pExpr6 :: Parser Expr
	pExpr6 = loc
		>>= \loc. (pNumber >>= \n. return (NumberExpr loc n))
			<<|> (pString >>= \s. return (StringExpr loc s))
			<<|> (pChar >>= \c. return (CharExpr loc c))
			<<|> (pBool >>= \b. return (BoolExpr loc b))
			<<|> (pSymbol '('
				>>| pExpr1
				>>= \sub. pSymbol ')'
				>>| return (Nested loc sub))
			<<|> pTuple
			<<|> pCaseExpr
			<<|> pAppl

	pTuple
	= loc
		>>= \loc. pSymbol '('
		>>| db "parsed (" pExpr1
		>>= \e1. some (pSymbol ',' >>| pExpr1)
		>>= \rest. pSymbol ')'
		>>| return (TupleExpr loc [e1:rest])

	pList :: Parser Expr
	pList = loc
		>>= \loc. (pSymbol '[' >>| pSymbol ']' >>| return (EmptyList loc))
		<<|> (pSymbol '['
			>>| pExpr1
			>>= \e. pSymbol ':'
			>>| pList
			>>= \es. pSymbol ']'
			>>| return (ListExpr loc e es))

	pAppl
	| debug "Parsing function application" = undef
	= loc
		>>= \loc. pIdentifier
		>>= \func. many pArgument
		>>= \args. return (FuncExpr loc func args)
	where
		pArgument = loc
			>>= \loc. (pNumber >>= \n. return (NumberExpr loc n))
				<<|> (pString >>= \s. return (StringExpr loc s))
				<<|> (pChar >>= \c. return (CharExpr loc c))
				<<|> (pBool >>= \b. return (BoolExpr loc b))
				<<|> pList
				<<|> pTuple
				<<|> (pIdentifier >>= \id. return (FuncExpr loc id []))
				<<|> (pSymbol '(' >>| pExpr1 >>= \re. pSymbol ')' >>| return (Nested loc re))

	pCaseExpr :: Parser Expr
	pCaseExpr
	| debug "Parse CaseExpr" = undef
	= loc
		>>= \loc. pSpecificIdentifier "match"
		>>| db "got match keyword" pExpr
		>>= \e. db ("Got expr to match: " + toString e) (strict (pSymbol ':') (\(l, t). General l ("Expected symbol ':' after case expression, got " +++ toString t)))
		>>| incIndent
		>>= \indent. strict (some (pCaseRule indent)) (\(l, t). General l ("Expected at least one case rule, got " +++ toString t))
		>>= \rules. decIndent
		>>| return (CaseExpr loc e rules)
	where
		pCaseRule :: Int -> Parser MatchRule
		pCaseRule currentIndent
		| debug "parsing case rule" = undef
		= pSymbol '\n'
			>>| db ("Parsing rule with indentation " + toString currentIndent) (pSymbols (join "" (repeatn currentIndent "\t")))
			>>| db "Got correct indentation" loc
			>>= \loc. pMatch
			>>= \match. db "Parsed match rule LHS" (pSymbols "->")
			>>| pExpr
			>>= \expr. return (MatchRule loc match expr)

pFGuard :: Parser FGuard
pFGuard
| debug "Parsing function guard" = undef
= (pSymbols "\n|"
		>>| loc
		>>= \geloc. strict pExpr (\(l, t). General l "Could not parse guard LHS")
		>>= \ge. db  "Parsed guard left" (pSymbol '=')
		>>| loc
		>>= \reloc. pExpr
		>>= \re. db "Parsed guard right" (pure (Guarded geloc (WExpr geloc ge) (WExpr reloc re))))
	<|> (optionalNewline
		>>| pSymbols "="
		>>| loc
		>>= \loc. pExpr
		>>= \e. pure (NonGuarded loc (WExpr loc e)))

pMatch :: Parser Match
pMatch
| debug "Parsing function match" = undef
= loc
	>>= \loc. (pIdentifier >>= \id. return (MVar loc id))
	<<|> (pString >>= \str. return (MString loc str))
	<<|> (pChar >>= \char. return (MChar loc char))
	<<|> (pBool >>= \bool. return (MBool loc bool))
	<<|> (pNumber >>= \num. return (MInt loc num))
	<<|> (pSymbol '(' >>| tupleEls >>= \els. pSymbol ')' >>| return (MTuple loc els))
	<<|> pMatchList loc
where
	tupleEls = pMatch
		>>= \e. some (pSymbol ',' >>| pMatch)
		>>= \es. return [e : es]

	pMatchList loc = (pSymbols "[]" >>| return (MEmptyList loc))
		<<|> (pSymbol '['
			>>| pMatch
			>>= \m. pSymbol ':'
			>>| pMatch
			>>= \es. pSymbol ']'
			>>| return (MList loc m es))

pFBody :: String -> Parser FBody
pFBody fname
| debug "Parsing function body" = undef
= 	loc
	>>= \loc. strict (pSpecificIdentifier fname) (\(l, t). General l ("Expected function body with name " + fname))
	>>| db "Correct function name" (many pMatch)
	>>= \arguments. db ("Parsed arguments [" + join " " (map toString arguments) + "]") (some pFGuard)
	>>= \guards. pure (FBody loc fname arguments guards)

pFDecl :: Parser FDecl
pFDecl
| debug "Parsing function declaration" = undef
=	loc
	>>= \loc. strict pIdentifier (\(l, t). General l ("Expected function name, got " + toString t))
	>>= \fName. db ("Got function name: " + fName) (strict (pSymbols "::") (\(l, t). General l ("Expected token \"::\", got " + toString t)))
	>>| db "Parsing type" strict pType (\(l, t). General l ("Expected function type, got " + toString t))
	>>= \fType. db ("Parsed type " + toString fType) (some ( pSymbol '\n' >>| pFBody fName))
	>>= \fBody. optionalNewline
	>>| pure (FDecl loc fName fType fBody)

pAst :: Parser AST
pAst = some (optionalNewline >>| pFDecl)
	>>= \fdecls. pure (AST fdecls)

parse :: [((Int, Int), Token)] -> MaybeError ParseError AST
parse inp
# initialState = {tokens = inp, indent = 0}
= case pAst of
	(Parser f) = case f initialState of
		[] = Error (General (0,0) "Parsing failed")
		[(Parsed ast, {tokens}):rest] = case tokens of
			[] = Ok ast
			[(l, t): _] = case filter isFail (map fst rest) of
				[] = Error (General l ("Unexpected token :" +++ toString t))
				[(Failed e):es] = (Error e)
		[(Failed e,_):_] = Error e

