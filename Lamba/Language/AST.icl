implementation module Lamba.Language.AST

import StdEnv, Text

instance toString Type
where
	toString TBool = "Bool"
	toString TInt = "Int"
	toString TChar = "Char"
	toString TString = "String"
	toString TVoid = "Void"
	toString (TFunc f t) = toString f + " -> " + toString t
	toString (TTuple els) = "(" + join ", " (map toString els) + ")"

instance toString Match
where
	toString (MVar s) = s 
	toString (MNum i) = toString i
	toString (MString s) = "\"" + s + "\""
	toString (MChar c) = "\' " + toString c + "\'"
	toString (MBool b) = toString b 
	toString (MTuple els) = "(" + join ", " (map toString els) + ")"

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
	toString (OrExpr e1 e2) = bracket (toString e1 + " || " + toString e2)
	toString (AndExpr e1 e2) = bracket (toString e1 + " && " + toString e2)
	toString (NotExpr e) = "!" + toString e
	toString (EqExpr e1 e2) = bracket (toString e1 + " == " + toString e2)
	toString (LeqExpr e1 e2) = bracket (toString e1 + " <= " + toString e2)
	toString (GeqExpr e1 e2) = bracket (toString e1 + " >= " + toString e2)
	toString (NeqExpr e1 e2) = bracket (toString e1 + " != " + toString e2)
	toString (LesserExpr e1 e2) = bracket (toString e1 + " < " + toString e2)
	toString (GreaterExpr e1 e2) = bracket (toString e1 + " > " + toString e2)
	toString (PlusExpr e1 e2) = bracket (toString e1 + " + " + toString e2)
	toString (MinusExpr e1 e2) = bracket (toString e1 + " - " + toString e2)
	toString (NegExpr e) = "-" + toString e 
	toString (TimesExpr e1 e2) = bracket (toString e1 + " * " + toString e2)
	toString (DivideExpr e1 e2) = bracket (toString e1 + " / " + toString e2)
	toString (ModuloExpr e1 e2) = bracket (toString e1 + " % " + toString e2)
	toString (NumberExpr n) = toString n
	toString (StringExpr s) = "\"" + s + "\""
	toString (CharExpr c) = "\'" + toString c + "\'"
	toString (BoolExpr b) = toString b
	toString (Nested e) = "(" + toString e + ")"
	toString (TupleExpr els) = "(" + join ", " (map toString els) + ")"
	toString (FuncExpr fName []) = fName
	toString (FuncExpr fName args) = fName + " " + join " " (map toString args)
		
bracket s = "(" + s + ")"
