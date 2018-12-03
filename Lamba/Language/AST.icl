implementation module Lamba.Language.AST

import StdEnv, Text, StdMaybe, Data.GenEq

derive gEq Type

instance == SourceLocation
where
	(==) l1 l2 = l1 === l2

instance < SourceLocation
where
	(<) (l1, c1) (l2, c2) = l1 < l2 || c1 < c2

instance == Type
where
	(==) t1 t2 = t1 === t2

instance toString Type
where
	toString TBool = "TBool"
	toString TInt = "TInt"
	toString TChar = "TChar"
	toString TString = "TString"
	toString TVoid = "TVoid"
	toString (TVar v) = "TVar " + toString v
	toString (TFunc f t) = bracket ("TFunc " + toString f + " " + toString t)
	toString (TTuple els) = bracket ("TTuple " + join ", " (map toString els))
	toString (TList t) = bracket ("TList " + toString t)

instance toString Match
where
	toString (MVar loc s) = bracket ("MVar " + s)
	toString (MInt loc i) = bracket ("MInt " + toString i)
	toString (MString loc s) = bracket ("MString " + s)
	toString (MChar loc c) = bracket ("MChar " + toString c)
	toString (MBool loc b) = bracket ("MBool " + toString b)
	toString (MTuple loc els) = bracket ("MTuple " + join " " (map toString els))
	toString (MEmptyList loc) = "MEmptyList"
	toString (MList loc e es) = bracket ("MList " + toString e + " " + toString es)

instance toString MatchRule
where
	toString (MatchRule loc match expr) = bracket (toString loc + " MatchRule " + toString match + " " + toString expr)

instance toString AST
where
	toString (AST functions) = join "\n" (map toString functions)

instance toString FDecl
where
	toString (FDecl _ name type bodies) = name
		+ " :: "
		+ toString type
		+ "\n"
		+ join "\n" (map (\b. name + toString b) bodies)
		+ "\n"

instance toString FBody
where
	toString (FBody _ _ [] guards) = concat (map toString guards)
	toString (FBody _ _ matches guards)
	= " " + join " " (map toString matches) + concat (map toString guards)

instance toString FGuard
where
	toString (NonGuarded _ e) = " = " + toString e
	toString (Guarded _ g e) = "\n| "
		+ toString g
		+ " = "
		+ toString e

instance toString Expr
where
	toString (OrExpr loc e1 e2) = bracket (toString loc + " OrExpr " + toString e1 + " " + toString e2)
	toString (AndExpr loc e1 e2) = bracket (toString loc + " AndExpr " + toString e1 + " " + toString e2)
	toString (NotExpr loc e) = bracket (toString loc + " NotExpr" + toString e)
	toString (EqExpr loc e1 e2) = bracket (toString loc + " EqExpr " + toString e1 + " " + toString e2)
	toString (LeqExpr loc e1 e2) = bracket (toString loc + " LeqExpr " + toString e1 + " " + toString e2)
	toString (GeqExpr loc e1 e2) = bracket (toString loc + " GeqExpr " + toString e1 + " " + toString e2)
	toString (NeqExpr loc e1 e2) = bracket (toString loc + " NeqExpr " + toString e1 + " " + toString e2)
	toString (LesserExpr loc e1 e2) = bracket (toString loc + " LesserExpr " + toString e1 + " " + toString e2)
	toString (GreaterExpr loc e1 e2) = bracket (toString loc + " GreaterExpr " + toString e1 + " " + toString e2)
	toString (PlusExpr loc e1 e2) = bracket (toString loc + " PlusExpr " + toString e1 + " " + toString e2)
	toString (MinusExpr loc e1 e2) = bracket (toString loc + " MinusExpr " + toString e1 + " " + toString e2)
	toString (NegExpr loc e) = bracket (toString loc + " NeqExpr " + toString e)
	toString (TimesExpr loc e1 e2) = bracket (toString loc + " TimesExpr " + toString e1 + " " + toString e2)
	toString (DivideExpr loc e1 e2) = bracket (toString loc + " DivideExpr " + toString e1 + " " + toString e2)
	toString (ModuloExpr loc e1 e2) = bracket (toString loc + " ModuloExpr " + toString e1 + " " + toString e2)
	toString (NumberExpr loc n) = bracket (toString loc + " NumberExpr " + toString n)
	toString (StringExpr loc s) = bracket (toString loc + " Stringexpr " + s)
	toString (CharExpr loc c) = toString loc + " \'" + toString c + "\'"
	toString (BoolExpr loc b) = bracket (toString loc + " BoolExpr " + toString b)
	toString (Nested loc e) = bracket (toString loc + " NestedExpr " + toString e)
	toString (TupleExpr loc els) = bracket (toString loc + " TupleExpr " + join " " (map toString els))
	toString (EmptyList loc) = toString loc + " EmptyList"
	toString (ListExpr loc e rest) = bracket (toString loc + " ListExpr " + toString e + " " + toString rest)
	toString (FuncExpr loc fName []) = bracket (toString loc + " VariableExpr " + fName)
	toString (FuncExpr loc fName args) = bracket (toString loc + " FuncExpr " + fName + " " + join " " (map toString args))
	toString (CaseExpr loc expr rules) = bracket (toString loc + " CaseExpr " + toString expr + " " + join " " (map toString rules))

instance toString WExpr
where
	toString (WExpr Nothing e) = toString e
	toString (WExpr (Just t) e) = bracket ("TypedExpr " + toString e + " [" + toString t + "]")

bracket s = "(" + s + ")"

instance toString SourceLocation
where
	toString (line, col) = "[" + toString line + ":" + toString col + "]"

arity :: Type -> Int
arity (TFunc f t) = inc (arity t)
arity _ = 0

returnType :: Type -> Type
returnType (TFunc f t) = returnType t
returnType t = t

toFunctionType :: [Type] -> Type
toFunctionType [f, t] = TFunc f t
toFunctionType [e:es] = TFunc e (toFunctionType es)
