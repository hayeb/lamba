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
# fEnv = 'DM'.fromList (map (\(FDecl loc name type _). (name, (loc, type))) decls)
# res = map (checkFunctionDeclaration fEnv) decls
= case fErrors res of
	[] = Ok ('DM'.toList (fResults res))
	es = Error es

checkFunctionDeclaration :: ITEnv FDecl
	-> MaybeError [TypeError] ITEnv
checkFunctionDeclaration fEnv (FDecl loc name type bodies)
| not (trace_tn ("Checking function declaration " + name)) = undef
# res = map (\b. checkFunctionBody name type b fEnv) bodies
= case fErrors res of
	[] = Ok (fResults res)
	es = Error es

checkFunctionBody :: String Type FBody ITEnv
	-> MaybeError [TypeError] ITEnv
checkFunctionBody name ft=:(TFunc _ _) (FBody loc args guards) env
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
checkFunctionBody name _ (FBody loc args=:[a:as] guards) env
	= singleError loc ("Function "
		+ name
		+ " does not take arguments, but found "
		+ toString (length args)
		+ " arguments")

checkFunctionBody name simpleType (FBody loc _ guards) env
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
checkGuard (NonGuarded loc expr) env t = trace_n ("Checking nonguard. Required type: " + toString t) (checkExpr loc expr t env)
checkGuard (Guarded loc guard expr) env t = trace_n "Checking guard" (comb (checkExpr loc guard TBool env) (checkExpr loc expr t env))

checkExpr :: SourceLocation Expr Type ITEnv -> MaybeError [TypeError] ITEnv
checkExpr loc (OrExpr e1 e2) TBool env = comb (checkExpr loc e1 TBool env) (checkExpr loc e2 TBool env)
checkExpr loc (AndExpr e1 e2) TBool env = comb (checkExpr loc e1 TBool env) (checkExpr loc e2 TBool env)

checkExpr loc (EqExpr e1 e2) TBool env
# results = [(r1, r2) \\ t <- [TInt, TChar, TBool, TString]
			, r1 <- [checkExpr loc e1 t env]
			, r2 <- [checkExpr loc e2 t env]]
= case filter (\(l, r). isOk l && isOk r) results of
	[] = singleError loc "Expected == operands of type Int, Char, Bool, String"
	_ = Ok 'DM'.newMap

checkExpr loc (NeqExpr e1 e2) TBool env = comb (checkExpr loc e1 TBool env) (checkExpr loc e2 TBool env)

checkExpr loc (LeqExpr e1 e2) TBool env = comb (checkExpr loc e1 TInt env) (checkExpr loc e2 TInt env)
checkExpr loc (GeqExpr e1 e2) TBool env = comb (checkExpr loc e1 TInt env) (checkExpr loc e2 TInt env)
checkExpr loc (LesserExpr e1 e2) TBool env = comb (checkExpr loc e1 TInt env) (checkExpr loc e2 TInt env)
checkExpr loc (GreaterExpr e1 e2) TBool env = comb (checkExpr loc e1 TInt env) (checkExpr loc e2 TInt env)
checkExpr loc (PlusExpr e1 e2) TInt env = comb (checkExpr loc e1 TInt env) (checkExpr loc e2 TInt env)
checkExpr loc (MinusExpr e1 e2) TInt env = comb (checkExpr loc e1 TInt env) (checkExpr loc e2 TInt env)
checkExpr loc (NegExpr e) TBool env = checkExpr loc e TBool env
checkExpr loc (TimesExpr e1 e2) TInt env = comb (checkExpr loc e1 TInt env) (checkExpr loc e2 TInt env)
checkExpr loc (DivideExpr e1 e2) TInt env = comb (checkExpr loc e1 TInt env) (checkExpr loc e2 TInt env)
checkExpr loc (ModuloExpr e1 e2) TInt env = comb (checkExpr loc e1 TInt env) (checkExpr loc e2 TInt env)
checkExpr loc (NumberExpr n) TInt env = Ok 'DM'.newMap
checkExpr loc (StringExpr s) TString env = Ok 'DM'.newMap
checkExpr loc (CharExpr c) TChar env = Ok 'DM'.newMap
checkExpr loc (BoolExpr b) TBool env = Ok 'DM'.newMap
checkExpr loc (Nested expr) t env = checkExpr loc expr t env
checkExpr loc (EmptyList) _ env = Ok 'DM'.newMap
checkExpr loc (ListExpr e es) (TList t) env
| not (trace_tn ("Checking list expression element " + toString e)) = undef
# res = checkExpr loc e t env
# res` = checkExpr loc es (TList t) env
= comb res res`

checkExpr loc (TupleExpr [e]) (TTuple [t]) env = checkExpr loc e t env
checkExpr loc (TupleExpr [e:es]) (TTuple [t:ts])  env
= comb (checkExpr loc e t env) (checkExpr loc (TupleExpr es) (TTuple ts) env)

checkExpr loc (FuncExpr name []) t env
| not (trace_tn ("Checking identifier " + name)) = undef
# idt = 'DM'.get name env
| isNothing idt = singleError loc ("Variable " + name + " is undefined")
# (_, type) = fromJust idt
| type == t = Ok 'DM'.newMap
= singleError loc ("Variable " + name + " has incorrect type. Expected:\n\t" + toString t + "\nGot:\n\t" + toString type)

checkExpr loc (FuncExpr f es) desiredType env
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
= checkFunctionApplication loc es functionType env

checkExpr loc e t _ =  singleError loc ("Expected expression of type " + toString t + ". Got: " + toString e)

checkFunctionApplication loc [] _ _ = Ok 'DM'.newMap
checkFunctionApplication loc [e:es] (TFunc f to) env
# res = checkExpr loc e f env
# res` = checkFunctionApplication loc es to env
= comb res res`

checkFunctionApplication loc [e:es] t env = singleError loc ("Expected function type for expression " + toString e + ", got " + toString t)




