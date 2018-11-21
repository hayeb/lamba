implementation module Lamba.TypeChecker

import StdEnv, Data.Error, Text

import Lamba.Language.AST

instance toString TypeError
where
	toString (TypeError (line, col) e) = "[" + toString line + ":" + toString col + "] Type error: " + e


comb :: (MaybeError [TypeError] TEnv) (MaybeError [TypeError] TEnv) -> MaybeError [TypeError] TEnv
comb (Error e) (Ok r) = Error e
comb (Error e1) (Error e2) = Error (e1 ++ e2)
comb (Ok l) (Error e) = Error e
comb (Ok l) (Ok r) = Ok (l ++ r)

fErrors :: [MaybeError [TypeError] TEnv] -> [TypeError]
fErrors mbErrors = flatten (map fromError (filter isError mbErrors))

fResults :: [MaybeError [TypeError] TEnv] -> TEnv
fResults mbResults = flatten (map fromOk (filter isOk mbResults))

singleError :: SourceLocation String -> MaybeError [TypeError] a
singleError loc msg = Error [TypeError loc msg]

typecheck :: AST -> MaybeError [TypeError] TEnv
typecheck (AST decls)
# fEnv = map (\(FDecl loc name type _). (name, loc, type)) decls 
# res = map (checkFunctionDeclaration fEnv) decls
= case fErrors res of
	[] = Ok (fResults res) 
	es = Error es

checkFunctionDeclaration :: TEnv FDecl 
	-> MaybeError [TypeError] TEnv
checkFunctionDeclaration fEnv (FDecl loc name type bodies)
# res = map (\b. checkFunctionBody name type b fEnv) bodies
= case fErrors res of
	[] = Ok (fResults res)
	es = Error es

checkFunctionBody :: String Type FBody TEnv
	-> MaybeError [TypeError] TEnv
// A function with arguments but no arguments were provided
checkFunctionBody name ft=:(TFunc _ _) (FBody loc args guards) env
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
	# env = env ++ env`
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
checkFunctionBody name simpleType (FBody loc _ guards) env = Ok []

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
	-> MaybeError [TypeError] TEnv
checkMatch loc (TFunc l _) [m]
= checkSpecificArgument loc l m

checkMatch loc (TFunc l r) [m : ms]
# res = checkSpecificArgument loc l m
# res` = checkMatch loc r ms
= case (res, res`) of
	(Error e, Ok _) = Error e
	(Error e1, Error e2) = Error (e1 ++ e2)
	(Ok _, Error e) = Error e
	(Ok l, Ok r) = Ok (l ++ r)
		
checkSpecificArgument :: SourceLocation Type Match
	-> MaybeError [TypeError] TEnv
// A variable can have any type
checkSpecificArgument loc t (MVar name) = Ok [(name, loc, t)]
// Tuples
checkSpecificArgument _ (TTuple []) (MTuple []) = Ok []
checkSpecificArgument loc (TTuple [e:es]) (MTuple [m:ms]) 
# res = checkSpecificArgument loc e m
# res` = checkSpecificArgument loc (TTuple es) (MTuple ms)
= case (res, res`) of
	(Error e, Ok _) = Error e
	(Error e1, Error e2) = Error (e1 ++ e2)
	(Ok _, Error e) = Error e
	(Ok l, Ok r) = Ok (l ++ r)
checkSpecificArgument loc (TTuple _) match 
	= singleError loc ("Expected argument of type Tuple, got " + toString match)

// Basic types
checkSpecificArgument loc TBool (MBool val) = Ok []
checkSpecificArgument loc TBool match 
	= singleError loc ("Expected match of type Bool, got " + toString match)

checkSpecificArgument loc TInt (MInt val) = Ok []
checkSpecificArgument loc TInt match 
	= singleError loc ("Expected match of type Int, got " + toString match)

checkSpecificArgument loc TChar (MChar val) = Ok []
checkSpecificArgument loc TChar match 
	= singleError loc ("Expected match of type Char, got " + toString match)

checkSpecificArgument loc TString (MString val) = Ok []
checkSpecificArgument loc TString match 
	= singleError loc ("Expected match of type String, got " + toString match)

checkGuard :: FGuard TEnv Type -> MaybeError [TypeError] TEnv
checkGuard (NonGuarded loc expr) env t = checkExpr loc expr t env
checkGuard (Guarded loc guard expr) env t = comb (checkExpr loc guard TBool env) (checkExpr loc expr t env)

checkExpr :: SourceLocation Expr Type TEnv -> MaybeError [TypeError] TEnv
checkExpr loc (OrExpr e1 e2) TBool env = comb (checkExpr loc e1 TBool env) (checkExpr loc e2 TBool env)
checkExpr loc (AndExpr e1 e2) TBool env = comb (checkExpr loc e1 TBool env) (checkExpr loc e2 TBool env)

// TODO: ?
checkExpr loc (EqExpr e1 e2) TBool env
# results = [(r1, r2) \\ t <- [TInt, TChar, TBool, TString]
			, r1 <- [checkExpr loc e1 t env]
			, r2 <- [checkExpr loc e2 t env]]
= case filter (\(l, r). isOk l && isOk r) results of 
	[] = singleError loc "Expected == operands of type Int, Char, Bool, String"
	_ = Ok []

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
checkExpr loc (NumberExpr n) TInt env = Ok []
checkExpr loc (StringExpr s) TString env = Ok []
checkExpr loc (CharExpr c) TChar env = Ok []
checkExpr loc (BoolExpr b) TBool env = Ok []
checkExpr loc (Nested expr) t env = checkExpr loc expr t env
// TODO: Tuples
checkExpr loc (TupleExpr es) (TTuple ts)  env = Ok []

// TODO: Functions
checkExpr loc (FuncExpr _ es) _ env = Ok []

checkExpr loc _ t _ =  singleError loc ("Expected expression of type " + toString t)








