definition module Lamba.Language.AST

import StdMaybe
import StdString

:: SourceLocation :== (Int, Int)

:: AST = AST [FDecl]

:: FDecl = FDecl SourceLocation String Type [FBody]

:: FBody = FBody SourceLocation String [Match] [FGuard]

:: FGuard = NonGuarded SourceLocation WExpr
	| Guarded SourceLocation WExpr WExpr

:: Type = TVar Int
	| TBool
	| TInt
	| TChar
	| TString
	| TVoid
	| TTuple [Type]
	| TList Type
	| TFunc Type Type

:: Match = MVar SourceLocation String
	| MInt SourceLocation Int
	| MString SourceLocation String
	| MChar SourceLocation Char
	| MBool SourceLocation Bool
	| MTuple SourceLocation [Match]
	| MEmptyList SourceLocation
	| MList SourceLocation Match Match

:: WExpr = WExpr (Maybe Type) Expr

:: Expr = OrExpr SourceLocation Expr Expr
	| AndExpr SourceLocation Expr Expr
	| NotExpr SourceLocation Expr
	| EqExpr SourceLocation Expr Expr
	| LeqExpr SourceLocation Expr Expr
	| GeqExpr SourceLocation Expr Expr
	| NeqExpr SourceLocation Expr Expr
	| LesserExpr SourceLocation Expr Expr
	| GreaterExpr SourceLocation Expr Expr
	| PlusExpr SourceLocation Expr Expr
	| MinusExpr SourceLocation Expr Expr
	| NegExpr SourceLocation Expr
	| TimesExpr SourceLocation Expr Expr
	| DivideExpr SourceLocation Expr Expr
	| ModuloExpr SourceLocation Expr Expr
	| NumberExpr SourceLocation Int
	| StringExpr SourceLocation String
	| CharExpr SourceLocation Char
	| BoolExpr SourceLocation Bool
	| Nested SourceLocation Expr
	| TupleExpr SourceLocation [Expr]
	| ListExpr SourceLocation Expr Expr
	| EmptyList SourceLocation
	| FuncExpr SourceLocation String [Expr]
	| CaseExpr SourceLocation Expr [MatchRule]

:: MatchRule = MatchRule SourceLocation Match Expr

instance toString Type, Match, AST, FDecl, FBody, FGuard, WExpr, Expr, MatchRule, SourceLocation
instance == Type, SourceLocation
instance < SourceLocation

arity :: Type -> Int
returnType :: Type -> Type
toFunctionType :: [Type] -> Type
