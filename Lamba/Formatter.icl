implementation module Lamba.Formatter

import StdEnv, Text
import Lamba.Language.AST

format :: AST -> String
format (AST decls) = join "\n\n" (map formatDecl decls)

formatDecl :: FDecl -> String
formatDecl (FDecl loc name (Just type) bodies) = name
	+ " :: "
	+ formatType type
	+ "\n"
	+ join "\n" (map formatBody bodies)

formatBody :: FBody -> String
formatBody (FBody loc name matches guards) = name
	+ " "
	+ (join " " (map formatMatch matches))
	+ (join "" (map formatGuard guards))

formatGuard :: FGuard -> String
formatGuard (NonGuarded loc expr) = " = " + formatWExpr expr
formatGuard (Guarded loc g expr) = "\n| " + formatWExpr g + " = " + formatWExpr expr

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
formatMatch (MVar loc s) = s
formatMatch (MInt loc i) = toString i
formatMatch (MString loc s) = "\"" + s + "\""
formatMatch (MChar loc c) = "\'" + toString c + "\'"
formatMatch (MBool loc b) = toString b
formatMatch (MTuple loc els) = "(" + join ", " (map formatMatch els) + ")"
formatMatch (MEmptyList loc) = "[]"
formatMatch (MList loc e es) = "[" + formatMatch e + ":" + formatMatch es + "]"

formatWExpr :: WExpr -> String
formatWExpr (WExpr _ e) = formatExpr e

formatExpr :: Expr -> String
formatExpr (OrExpr _ e1 e2) = bracket (formatExpr e1 + " || " + formatExpr e2)
formatExpr (AndExpr _ e1 e2) = bracket (formatExpr e1 + " && " + formatExpr e2)
formatExpr (NotExpr _ e) = "!" + formatExpr e
formatExpr (EqExpr _ e1 e2) = bracket (formatExpr e1 + " == " + formatExpr e2)
formatExpr (LeqExpr _ e1 e2) = bracket (formatExpr e1 + " <= " + formatExpr e2)
formatExpr (GeqExpr _ e1 e2) = bracket (formatExpr e1 + " >= " + formatExpr e2)
formatExpr (NeqExpr _ e1 e2) = bracket (formatExpr e1 + " != " + formatExpr e2)
formatExpr (LesserExpr _ e1 e2) = bracket (formatExpr e1 + " < " + formatExpr e2)
formatExpr (GreaterExpr _ e1 e2) = bracket (formatExpr e1 + " > " + formatExpr e2)
formatExpr (PlusExpr _ e1 e2) = bracket (formatExpr e1 + " + " + formatExpr e2)
formatExpr (MinusExpr _ e1 e2) = bracket (formatExpr e1 + " - " + formatExpr e2)
formatExpr (NegExpr _ e) = "-" + formatExpr e
formatExpr (TimesExpr _ e1 e2) = bracket (formatExpr e1 + " * " + formatExpr e2)
formatExpr (DivideExpr _ e1 e2) = bracket (formatExpr e1 + " / " + formatExpr e2)
formatExpr (ModuloExpr _ e1 e2) = bracket (formatExpr e1 + " % " + formatExpr e2)
formatExpr (NumberExpr _ n) = toString n
formatExpr (StringExpr _ s) = "\"" + s + "\""
formatExpr (CharExpr _ c) = "\'" + toString c + "\'"
formatExpr (BoolExpr _ b) = toString b
formatExpr (Nested _ e) = "(" + formatExpr e + ")"
formatExpr (TupleExpr _ els) = "(" + join ", " (map formatExpr els) + ")"
formatExpr (EmptyList _) = "[]"
formatExpr (ListExpr _ e rest) = "[" + formatExpr e + ":" + formatExpr rest + "]"
formatExpr (FuncExpr _ fName []) = fName
formatExpr (FuncExpr _ fName args) = fName + " " + join " " (map formatExpr args)

bracket s = "(" + s + ")"
