implementation module Lamba.Language.AST

import StdEnv, Text

instance == Type
where
	(==) TBool TBool = True
	(==) TInt TInt = True
	(==) TChar TChar = True
	(==) TString TString = True
	(==) TVoid TVoid = True
	(==) (TFunc f1 t1) (TFunc f2 t2) = f1 == f2 && t1 == t2
	(==) (TTuple els1) (TTuple els2) = els1 == els2
	(==) (TList t1) (TList t2) = t1 == t2
	(==) _ _ = False

instance toString Type
where
	toString TBool = "TBool"
	toString TInt = "TInt"
	toString TChar = "TChar"
	toString TString = "TString"
	toString TVoid = "TVoid"
	toString (TFunc f t) = bracket ("TFunc " + toString f + " " + toString t)
	toString (TTuple els) = bracket ("TTuple " + join ", " (map toString els))
	toString (TList t) = bracket ("TList" + toString t)

instance toString Match
where
	toString (MVar s) = bracket ("MVar " + s)
	toString (MInt i) = bracket ("MInt " + toString i)
	toString (MString s) = bracket ("MString " + s)
	toString (MChar c) = bracket ("MChar " + toString c)
	toString (MBool b) = bracket ("MBool " + toString b)
	toString (MTuple els) = bracket ("MTuple " + join " " (map toString els))
	toString MEmptyList = "MEmptyList"
	toString (MList e es) = bracket ("MList " + toString e + " " + toString es)

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
	toString (FBody _ [] guards) = concat (map toString guards)
	toString (FBody _ matches guards)
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
	toString (OrExpr e1 e2) = bracket ("OrExpr " + toString e1 + " " + toString e2)
	toString (AndExpr e1 e2) = bracket ("AndExpr " + toString e1 + " " + toString e2)
	toString (NotExpr e) = bracket ("NotExpr" + toString e)
	toString (EqExpr e1 e2) = bracket ("EqExpr " + toString e1 + " " + toString e2)
	toString (LeqExpr e1 e2) = bracket ("LeqExpr " + toString e1 + " " + toString e2)
	toString (GeqExpr e1 e2) = bracket ("GeqExpr " + toString e1 + " " + toString e2)
	toString (NeqExpr e1 e2) = bracket ("NeqExpr " + toString e1 + " " + toString e2)
	toString (LesserExpr e1 e2) = bracket ("LesserExpr " + toString e1 + " " + toString e2)
	toString (GreaterExpr e1 e2) = bracket ("GreaterExpr " + toString e1 + " " + toString e2)
	toString (PlusExpr e1 e2) = bracket ("PlusExpr " + toString e1 + " " + toString e2)
	toString (MinusExpr e1 e2) = bracket ("MinusExpr " + toString e1 + " " + toString e2)
	toString (NegExpr e) = bracket ("NeqExpr " + toString e)
	toString (TimesExpr e1 e2) = bracket ("TimesExpr " + toString e1 + " " + toString e2)
	toString (DivideExpr e1 e2) = bracket ("DivideExpr " + toString e1 + " " + toString e2)
	toString (ModuloExpr e1 e2) = bracket ("ModuloExpr " + toString e1 + " " + toString e2)
	toString (NumberExpr n) = bracket ("NumberExpr " + toString n)
	toString (StringExpr s) = bracket ("Stringexpr " + s)
	toString (CharExpr c) = "\'" + toString c + "\'"
	toString (BoolExpr b) = bracket ("BoolExpr " + toString b)
	toString (Nested e) = bracket ("NestedExpr " + toString e)
	toString (TupleExpr els) = bracket ("TupleExpr " + join " " (map toString els))
	toString EmptyList = "EmptyList"
	toString (ListExpr e rest) = bracket ("ListExpr " + toString e + " " + toString rest)
	toString (FuncExpr fName []) = bracket ("VariableExpr " + fName)
	toString (FuncExpr fName args) = bracket ("FuncExpr " + fName + " " + join " " (map toString args))
		
bracket s = "(" + s + ")"
