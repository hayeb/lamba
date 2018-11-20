definition module Lamba.Language.AST

import StdEnv

:: SourceLocation :== (Int, Int)

:: AST = AST [FDecl]

:: FDecl = FDecl SourceLocation String Type [FBody]

:: FBody = FBody SourceLocation [Match] [FGuard]

:: FGuard = NonGuarded SourceLocation Expr
	| Guarded SourceLocation Expr Expr

:: Type = TBool
	| TInt
	| TChar
	| TString
	| TVoid
	| TTuple [Type]
	| TFunc Type Type

:: Match = MVar String
	| MNum Int
	| MString String
	| MChar Char
	| MBool Bool
	| MTuple [Match]

:: Expr = OrExpr Expr Expr
	| AndExpr Expr Expr
	| NotExpr Expr
	| EqExpr Expr Expr
	| LeqExpr Expr Expr
	| GeqExpr Expr Expr
	| NeqExpr Expr Expr
	| LesserExpr Expr Expr
	| GreaterExpr Expr Expr
	| PlusExpr Expr Expr
	| MinusExpr Expr Expr
	| NegExpr Expr
	| TimesExpr Expr Expr
	| DivideExpr Expr Expr
	| ModuloExpr Expr Expr
	| NumberExpr Int
	| StringExpr String
	| CharExpr Char
	| BoolExpr Bool
	| Nested Expr
	| TupleExpr [Expr]
	| FuncExpr String [Expr]

instance toString Type, Match, AST, FDecl, FBody, FGuard, Expr
