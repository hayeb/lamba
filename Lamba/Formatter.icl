implementation module Lamba.Formatter

import StdEnv, Text
import Lamba.Language.AST

format :: AST -> String
format (AST decls) = join "\n\n" (map formatDecl decls)

formatDecl :: FDecl -> String
formatDecl (FDecl loc name type bodies) = name 
	+ " :: " 
	+ formatType type
	+ "\n"
	+ (join "\n" (map (formatBody name) bodies))

formatBody :: String FBody -> String
formatBody name (FBody loc matches guards) = name 
	+ " "
	+ (join " " (map formatMatch matches))
	+ (join "" (map formatGuard guards))

formatGuard :: FGuard -> String
formatGuard (NonGuarded loc expr) = " = " + formatExpr expr
formatGuard (Guarded loc g expr) = "\n| " + formatExpr g + " = " + formatExpr expr

formatType :: Type -> String
formatType TBool = "Bool"
formatType TInt = "Int"
formatType TChar = "Char"
formatType TString = "String"
formatType TVoid = "Void"
formatType (TFunc f t) = formatType f + " -> " + formatType t
formatType (TTuple els) = "(" + join ", " (map formatType els) + ")"
formatType (TList t) = "[" + formatType t + "]"

formatMatch :: Match -> String
formatMatch (MVar s) = s 
formatMatch (MInt i) = toString i
formatMatch (MString s) = "\"" + s + "\""
formatMatch (MChar c) = "\'" + toString c + "\'"
formatMatch (MBool b) = toString b 
formatMatch (MTuple els) = "(" + join ", " (map formatMatch els) + ")"
formatMatch MEmptyList = "[]"
formatMatch (MList e es) = "[" + formatMatch e + ":" + formatMatch es + "]"

formatExpr :: Expr -> String 
formatExpr (OrExpr e1 e2) = bracket (formatExpr e1 + " || " + formatExpr e2)
formatExpr (AndExpr e1 e2) = bracket (formatExpr e1 + " && " + formatExpr e2)
formatExpr (NotExpr e) = "!" + formatExpr e
formatExpr (EqExpr e1 e2) = bracket (formatExpr e1 + " == " + formatExpr e2)
formatExpr (LeqExpr e1 e2) = bracket (formatExpr e1 + " <= " + formatExpr e2)
formatExpr (GeqExpr e1 e2) = bracket (formatExpr e1 + " >= " + formatExpr e2)
formatExpr (NeqExpr e1 e2) = bracket (formatExpr e1 + " != " + formatExpr e2)
formatExpr (LesserExpr e1 e2) = bracket (formatExpr e1 + " < " + formatExpr e2)
formatExpr (GreaterExpr e1 e2) = bracket (formatExpr e1 + " > " + formatExpr e2)
formatExpr (PlusExpr e1 e2) = bracket (formatExpr e1 + " + " + formatExpr e2)
formatExpr (MinusExpr e1 e2) = bracket (formatExpr e1 + " - " + formatExpr e2)
formatExpr (NegExpr e) = "-" + formatExpr e 
formatExpr (TimesExpr e1 e2) = bracket (formatExpr e1 + " * " + formatExpr e2)
formatExpr (DivideExpr e1 e2) = bracket (formatExpr e1 + " / " + formatExpr e2)
formatExpr (ModuloExpr e1 e2) = bracket (formatExpr e1 + " % " + formatExpr e2)
formatExpr (NumberExpr n) = toString n
formatExpr (StringExpr s) = "\"" + s + "\""
formatExpr (CharExpr c) = "\'" + toString c + "\'"
formatExpr (BoolExpr b) = toString b
formatExpr (Nested e) = "(" + formatExpr e + ")"
formatExpr (TupleExpr els) = "(" + join ", " (map formatExpr els) + ")"
formatExpr EmptyList = "[]"
formatExpr (ListExpr e rest) = "[" + formatExpr e + ":" + formatExpr rest + "]"
formatExpr (FuncExpr fName []) = fName
formatExpr (FuncExpr fName args) = fName + " " + join " " (map formatExpr args)
		
bracket s = "(" + s + ")"
