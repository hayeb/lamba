implementation module Lamba.TypeChecker

import StdEnv, Data.Error, Text, Data.Maybe, StdMisc, StdDebug
import qualified Data.Map as DM

import Lamba.Language.AST

instance toString TypeError
where
	toString (TypeError (line, col) e) = "[" + toString line + ":" + toString col + "] Type error: " + e

:: ITEnv :== 'DM'.Map String (SourceLocation, Type)

comb :: (MaybeError [TypeError] ITEnv) (MaybeError [TypeError] ITEnv) -> MaybeError [TypeError] ITEnv
comb (Error e) (Ok r) = Error e
comb (Error e1) (Error e2) = Error (e1 ++ e2)
comb (Ok l) (Error e) = Error e
comb (Ok l) (Ok r) = Ok ('DM'.union l r)

fErrors :: [MaybeError [TypeError] ITEnv] -> [TypeError]
fErrors mbErrors = flatten (map fromError (filter isError mbErrors))

fResults :: [MaybeError [TypeError] ITEnv] -> ITEnv
fResults mbResults = foldl 'DM'.union 'DM'.newMap (map fromOk (filter isOk mbResults))

singleError :: SourceLocation String -> MaybeError [TypeError] a
singleError loc msg = Error [TypeError loc msg]

typecheck :: AST -> MaybeError [TypeError] TEnv
typecheck (AST decls)
# fEnv = 'DM'.fromList (map (\(FDecl loc name (Just type) _). (name, (loc, type))) decls)
# res = map (checkFunctionDeclaration fEnv) decls
= case fErrors res of
	[] = Ok ('DM'.toList (fResults res))
	es = Error es

checkFunctionDeclaration :: ITEnv FDecl
	-> MaybeError [TypeError] ITEnv
checkFunctionDeclaration fEnv (FDecl loc name (Just type) bodies)
| not (trace_tn ("Checking function declaration " + name)) = undef
# res = map (\b. checkFunctionBody type b fEnv) bodies
= case fErrors res of
	[] = Ok (fResults res)
	es = Error es

checkFunctionBody :: Type FBody ITEnv
	-> MaybeError [TypeError] ITEnv
checkFunctionBody ft=:(TFunc _ _) (FBody loc name args guards) env
| not (trace_tn "Checking function body") = undef
| not (correctArity ft args)
	= singleError loc ("Function body "
		+ name
		+ " does not have the correct number of arguments. (Required: "
		+ toString (arity ft)
		+ ", found: "
		+ toString (length args)
		+ ")")
= case checkMatch loc ft args of
	Error e = Error e
	Ok env`
	# env = 'DM'.union env env`
	# res = map (\g. checkGuard g env (returnType ft)) guards
	= case fErrors res of
		[] = Ok (fResults res)
		es = Error es

// A simple function which takes no arguments, but arguments were specified.
checkFunctionBody _ (FBody loc name args=:[a:as] guards) env
	= singleError loc ("Function "
		+ name
		+ " does not take arguments, but found "
		+ toString (length args)
		+ " arguments")

checkFunctionBody simpleType (FBody loc name _ guards) env
| not (trace_tn ("Checking function body " + name + " with simple type " + toString simpleType)) = undef
# res = map (\g. checkGuard g env simpleType) guards
= case fErrors res of
	[] = Ok (fResults res)
	es = Error es

correctArity :: Type [Match] -> Bool
correctArity (TFunc _ _) [] = False
correctArity _ [] = True
correctArity (TFunc l r) [a : as] = correctArity r as
correctArity _ [a : as] = False

arity :: Type -> Int
arity (TFunc l r) = inc (arity r)
arity _ = 0

returnType :: Type -> Type
returnType (TFunc _ t) = returnType t
returnType t = t

checkMatch :: SourceLocation Type [Match]
	-> MaybeError [TypeError] ITEnv
checkMatch loc (TFunc l _) [m]
= checkSpecificArgument loc l m

checkMatch loc (TFunc l r) [m : ms]
# res = checkSpecificArgument loc l m
# res` = checkMatch loc r ms
= comb res res`

checkSpecificArgument :: SourceLocation Type Match
	-> MaybeError [TypeError] ITEnv
// A variable can have any type
checkSpecificArgument loc t (MVar name)
| not (trace_tn ("Var " + name + " has type " + toString t)) = undef
= Ok ('DM'.fromList [(name, (loc, t))])
// Lists
checkSpecificArgument loc (TList type) MEmptyList = Ok 'DM'.newMap
checkSpecificArgument loc (TList type) (MList e es)
# res = checkSpecificArgument loc type e
# res` = checkSpecificArgument loc (TList type) es
= comb res res`

// Tuples
checkSpecificArgument _ (TTuple []) (MTuple []) = Ok 'DM'.newMap
checkSpecificArgument loc (TTuple [e:es]) (MTuple [m:ms])
# res = checkSpecificArgument loc e m
# res` = checkSpecificArgument loc (TTuple es) (MTuple ms)
= comb res res`

checkSpecificArgument loc (TTuple _) match
	= singleError loc ("Expected argument of type Tuple, got " + toString match)

// Basic types
checkSpecificArgument loc TBool (MBool val) = Ok 'DM'.newMap
checkSpecificArgument loc TBool match
	= singleError loc ("Expected match of type Bool, got " + toString match)

checkSpecificArgument loc TInt (MInt val) = Ok 'DM'.newMap
checkSpecificArgument loc TInt match
	= singleError loc ("Expected match of type Int, got " + toString match)

checkSpecificArgument loc TChar (MChar val) = Ok 'DM'.newMap
checkSpecificArgument loc TChar match
	= singleError loc ("Expected match of type Char, got " + toString match)

checkSpecificArgument loc TString (MString val) = Ok 'DM'.newMap
checkSpecificArgument loc TString match
	= singleError loc ("Expected match of type String, got " + toString match)

checkGuard :: FGuard ITEnv Type -> MaybeError [TypeError] ITEnv
checkGuard (NonGuarded loc (WExpr _ expr)) env t = trace_n ("Checking nonguard. Required type: " + toString t) (checkExpr expr t env)
checkGuard (Guarded loc (WExpr _ guard) (WExpr _ expr)) env t = trace_n "Checking guard" (comb (checkExpr guard TBool env) (checkExpr expr t env))

checkExpr :: Expr Type ITEnv -> MaybeError [TypeError] ITEnv
checkExpr (OrExpr loc e1 e2) TBool env = comb (checkExpr e1 TBool env) (checkExpr e2 TBool env)
checkExpr (AndExpr loc e1 e2) TBool env = comb (checkExpr e1 TBool env) (checkExpr e2 TBool env)

checkExpr (EqExpr loc e1 e2) TBool env
# results = [(r1, r2) \\ t <- [TInt, TChar, TBool, TString]
			, r1 <- [checkExpr e1 t env]
			, r2 <- [checkExpr e2 t env]]
= case filter (\(l, r). isOk l && isOk r) results of
	[] = singleError loc "Expected == operands of type Int, Char, Bool, String"
	_ = Ok 'DM'.newMap

checkExpr (NeqExpr loc e1 e2) TBool env = comb (checkExpr e1 TBool env) (checkExpr e2 TBool env)

checkExpr (LeqExpr loc e1 e2) TBool env = comb (checkExpr e1 TInt env) (checkExpr e2 TInt env)
checkExpr (GeqExpr loc e1 e2) TBool env = comb (checkExpr e1 TInt env) (checkExpr e2 TInt env)
checkExpr (LesserExpr loc e1 e2) TBool env = comb (checkExpr e1 TInt env) (checkExpr e2 TInt env)
checkExpr (GreaterExpr loc e1 e2) TBool env = comb (checkExpr e1 TInt env) (checkExpr e2 TInt env)
checkExpr (PlusExpr loc e1 e2) TInt env = comb (checkExpr e1 TInt env) (checkExpr e2 TInt env)
checkExpr (MinusExpr loc e1 e2) TInt env = comb (checkExpr e1 TInt env) (checkExpr e2 TInt env)
checkExpr (NegExpr loc e) TBool env = checkExpr e TBool env
checkExpr (TimesExpr loc e1 e2) TInt env = comb (checkExpr e1 TInt env) (checkExpr e2 TInt env)
checkExpr (DivideExpr loc e1 e2) TInt env = comb (checkExpr e1 TInt env) (checkExpr e2 TInt env)
checkExpr (ModuloExpr loc e1 e2) TInt env = comb (checkExpr e1 TInt env) (checkExpr e2 TInt env)
checkExpr (CharExpr loc c) TChar env = Ok 'DM'.newMap
checkExpr (BoolExpr loc b) TBool env = Ok 'DM'.newMap
checkExpr (Nested loc expr) t env = checkExpr expr t env
checkExpr (EmptyList loc) _ env = Ok 'DM'.newMap
checkExpr (ListExpr loc e es) (TList t) env
| not (trace_tn ("Checking list expression element " + toString e)) = undef
# res = checkExpr e t env
# res` = checkExpr es (TList t) env
= comb res res`

checkExpr (TupleExpr loc [e]) (TTuple [t]) env = checkExpr e t env
checkExpr (TupleExpr loc [e:es]) (TTuple [t:ts])  env
= comb (checkExpr e t env) (checkExpr (TupleExpr loc es) (TTuple ts) env)

checkExpr (FuncExpr loc name []) t env
| not (trace_tn ("Checking identifier " + name)) = undef
# idt = 'DM'.get name env
| isNothing idt = singleError loc ("Variable " + name + " is undefined")
# (_, type) = fromJust idt
| type == t = Ok 'DM'.newMap
= singleError loc ("Variable " + name + " has incorrect type. Expected:\n\t" + toString t + "\nGot:\n\t" + toString type)

checkExpr (FuncExpr loc f es) desiredType env
| not (trace_tn ("Checking function " + f)) = undef
# ft = 'DM'.get f env
| isNothing ft = singleError loc ("Function " + f + " is undefined")
# (loc, functionType) = fromJust ft
| desiredType <> returnType functionType = singleError loc ("Function application "
	+ f
	+ " has incorrect return type. Expected:\n\t"
	+ toString desiredType
	+ "\nGot\n\t"
	+ toString (returnType functionType))
= checkFunctionApplication es functionType env

checkExpr e t _ =  singleError (0,0) ("Expected expression of type " + toString t + ". Got: " + toString e)

checkFunctionApplication [] _ _ = Ok 'DM'.newMap
checkFunctionApplication [e:es] (TFunc f to) env
# res = checkExpr e f env
# res` = checkFunctionApplication es to env
= comb res res`

checkFunctionApplication [e:es] t env = singleError (0,0) ("Expected function type for expression " + toString e + ", got " + toString t)

