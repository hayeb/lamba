implementation module Lamba.CodeGenerator

import StdInt, StdTuple

import Control.Applicative
import Data.Functor, Data.Tuple, Data.Map, Data.Error, Data.Func
import Text

import Lamba.Language.AST

from StdList import ++, map, indexList, zip2, flatten, reverse
from Control.Monad import class Monad(..), mapM
from Data.List import tail

:: CGState = CGState Int (Map String Substitution)
:: CG a = CG (CGState -> (MaybeError CodeGenError a, CGState))

:: FVarMap :== Map String (SourceLocation, Type, Bool)

:: Substitution = SVariable String
	| SInt Int
	| SString String
	| SChar Char
	| SBool Bool

instance pure CG
where
	pure v = CG \state. (Ok v, state)

instance Functor CG
where
	fmap f (CG a) = CG \state. appFst (fmap f) (a state)

instance <*> CG
where
	(<*>) (CG l) (CG r) = CG \state. case l state of
		(Error e, state) = (Error e, state)
		(Ok ll, state) = case r state of
			(Error e, state) = (Error e, state)
			(Ok rr, state) = (Ok (ll rr), state)

instance Monad CG
where
	bind (CG a) fa = CG \state. case a state of
		(Error e, state) = (Error e, state)
		(Ok aa, state) = case fa aa of
			(CG ffa) = ffa state

error error = CG \state. (Error error, state)

newLocal :: CG String
newLocal = CG \(CGState newvar subs). (Ok ("loc" + toString newvar), CGState (inc newvar) subs)

indent lines = map ((+) "\t") lines

addLabel label cs = [label + ":" : cs]

getSubstitution :: String -> CG Substitution
getSubstitution name = CG \(CGState newvar subs). (Ok (getSubstitution` name $ toList subs), CGState newvar subs)
where
	getSubstitution` name [] = SVariable name
	getSubstitution` name [(n, sub) : subs]
	| name == n = case sub of
		SVariable var = getSubstitution` var subs
		sub = sub
	= getSubstitution` name subs

addSubstitution name sub = CG \(CGState newvar subs). (Ok (), CGState newvar (put name sub subs))

substitutionToConstant (SVariable name) = "%" + name
substitutionToConstant (SInt n) = toString n
substitutionToConstant (SString s) = s
substitutionToConstant (SChar c) = toString (toInt c)
substitutionToConstant (SBool True) = "1"
substitutionToConstant (SBool False) = "0"

toLLVMType TBool = "i1"
toLLVMType TInt = "i64"
toLLVMType TChar = "i8"
// String is a structure:
// { i64 ; length of the string
// , i8* ; start of the string
// }
toLLVMType TString = "type {i64, i8*}*"
toLLVMType (TTuple els)
# elTypes = map toLLVMType els
= "type {" + join ", " elTypes +  "}*"
toLLVMType (TList t) = "type {" + toLLVMType t + ", i64*}*"

collectVariables :: [Match] [Type] [String] FVarMap -> (FVarMap, [(String, Substitution)], [String])
// Simple match expressions do not introduce variables
collectVariables [] [] _ m = (m, [], [])
collectVariables [match:ms] [t:ts] [arg:args] m
# (m, subs1, introduceCode1) = collectVariable match t m
# (m, subs, introduceCode) = collectVariables ms ts args m
= (m, subs1 ++ subs, introduceCode1 ++ introduceCode)
where
	collectVariable match type m = collectVariable` match type [] arg m
	where
		collectVariable` (MVar loc varname) t introCode result m = (put varname (loc, t, True) m,[(varname, SVariable result)], introCode)
		// collectVariable (MTuple _ els) (TTuple tels) m = foldr (\(t, e) m. collectVariable e t m) m (zip2 tels els)
		// collectVariable (MList _ hd tl) (TList t) m = collectVariable tl (TList t) (collectVariable hd t m)
		collectVariable` _ _ _ _ m = (m, [], [])

header = ["declare void @llvm.trap() cold noreturn nounwind"
		]

generateCode :: AST (Map String (SourceLocation, Type)) -> MaybeError CodeGenError [String]
generateCode ast types
# state = CGState 0 newMap
= case genAST ast $ fromList (toFVarMap (toList types)) of
	(CG f) = fst (f state)
where
	toFVarMap [] = []
	toFVarMap [(s, (l, t)) : r] = [(s, (l, t, False)) : toFVarMap r]

genAST :: AST FVarMap -> CG [String]
genAST (AST decls) m = mapM (\b. genDecl b m) decls
	>>= \declsCode. return (header ++ flatten declsCode)

genDecl :: FDecl FVarMap -> CG [String]
genDecl (FDecl loc name type bodies) m
# llvmReturnType = toLLVMType (returnType type)
# llvmArguments = map toLLVMType (withoutReturnType type)
# llvmArgumentsIndexed = zip2 (indexList llvmArguments) llvmArguments
# fName = "define " + llvmReturnType + " @" + name  + "(" + join ", " (map genParam llvmArgumentsIndexed) + ")"
= genBodies bodies
	>>= \bodies. return ([fName + " {"] ++ bodies ++ noMatchCode ++ ["}"])
where
	/* For every body:
	 * 1. Generate the match code
	 * 2. Introduce variables from the match
	 *
	 * Executing a function:
	 * 1. Start at the first match code
	 * 2. When it matches, jump to the first body code
	 * 3. When it does not matche, jump to the next body
	 * 4. When the final match does not match, jump to the "match exception" code.
	 */
	noMatchCode = ["noMatchLabel:", "\tcall void @llvm.trap()", "\tunreachable"]

	genParam (index, type) = type + " %arg" + toString index

	genBodies bodies = genBodies` bodies matchLabels bodyLabels
	where
		matchLabels = [ "match" + toString n \\ n <- [0..]]
		bodyLabels = ["body" + toString n \\ n <- [0..]]

		// The final body. When the match does not match, jump to the match exception code.
		genBodies` [FBody _ name matches guards] [matchLabel:_] [bodyLabel:_] = genBody matches guards matchLabel "noMatchLabel" bodyLabel
		genBodies` [FBody _ _ matches guards : bs] [matchLabel, nextMatchLabel : ms] [bodyLabel : bodyLabels]
			= genBody matches guards matchLabel (nextMatchLabel + "arg0") bodyLabel
				>>= \body. genBodies` bs [nextMatchLabel : ms] bodyLabels
				>>= \bodies. return (body ++ bodies)

		// Generated the body (matches + guards) for a single function body.
		genBody :: [Match] [FGuard] String String String -> CG [String]
		genBody matches guards matchLabel nextMatchLabel bodyLabel
		# args = ["arg" + toString n \\ n<- [0..]]
		= genMatches matches args matchLabel nextMatchLabel bodyLabel
			>>= \matchCode. return (collectVariables matches (withoutReturnType type) args m)
			>>= \(stateWithVariables, subs, introduceCode). mapM (\(name, sub). addSubstitution name sub) subs
			>>| genGuards guards stateWithVariables
			>>= \bodyCode. return (matchCode ++ addLabel bodyLabel (introduceCode ++ (tail bodyCode)))

		// Generates the match code for a single function body.
		genMatches :: [Match] [String] String String String -> CG [String]
		genMatches [] _ _ _ bodyLabel = return $ indent ["br label %" + bodyLabel]
		genMatches [match] [arg:_] currentMatchLabel nextMatchLabel bodyLabel = genMatch (arg, match) m
			>>= \(matchCode, result). getSubstitution result
			>>= \resultSub. return (addLabel (currentMatchLabel + arg) (indent (matchCode ++ ["br i1 " + substitutionToConstant resultSub + ", label %" + bodyLabel + ", label %" + nextMatchLabel])))
		genMatches [match : ms] [arg, nextArg : args] currentMatchLabel nextMatchLabel bodyLabel = genMatch (arg, match) m
			>>= \(matchCode, result). genMatches ms [nextArg : args] currentMatchLabel nextMatchLabel bodyLabel
			>>= \matchCodes. getSubstitution result
			>>= \resultSub. return (addLabel (currentMatchLabel + arg) (indent (matchCode ++ ["br i1 " + substitutionToConstant resultSub + ", label %" + currentMatchLabel + nextArg + ", label %" + nextMatchLabel]) ++ matchCodes))


genGuards :: [FGuard] FVarMap -> CG [String]
genGuards guards m = genGuards` guards m guardLabels
where
	noMatchingGuardCode = ["\tllvm.trap()"]
	guardLabels = ["guard" + toString n \\ n <- [0..]]

	genGuards` [] _ _ = return noMatchingGuardCode
	// A guard without a condition. Does not introduct variables, so jump directly to the body.
	genGuards` [(NonGuarded loc (WExpr _ e)) : gs] m [gl : _] = genExpr e m
		>>= \(ecode, result). getSubstitution result
		>>= \sub. return $ [gl + ":"] ++ indent (ecode ++ ["ret i64 " + substitutionToConstant sub])

	genGuards` [(Guarded loc (WExpr _ condition) (WExpr _ e)) : gs] m [gl, ngl : gls] = genExpr condition m
		>>= \(conditionCode, conditionResult). getSubstitution conditionResult
		>>= \conditionSub. genExpr e m
		>>= \(bodyCode, bodyResult). getSubstitution bodyResult
		>>= \bodySub. genGuards` gs m [ngl:gls]
		>>= \otherGuardsCode. return $
				[gl + ":"] ++ indent (conditionCode ++ ["br i1 " + substitutionToConstant conditionSub + ", label %" + gl + "__true, label %" + ngl])
			++ [gl + "__true:"] ++ indent (bodyCode ++ ["ret i64 " + substitutionToConstant bodySub])
			++ otherGuardsCode

genMatch :: (String, Match) FVarMap -> CG ([String], String)
// (match variable, this label, match label, fail label)
genMatch (arg, match) m = genMatch` match
where
	genMatch` (MInt loc n) = newLocal
		>>= \local. return (["%" + local + " = icmp eq i64 %" + arg + ", " + toString n], local)

	genMatch` (MVar loc _) = newLocal
		>>= \local. addSubstitution local (SInt 1)
		>>| return ([], local)

// (Code, result identifier)
genExpr :: Expr FVarMap -> CG ([String], String)
genExpr (NumberExpr loc n) _ = newLocal
	>>= \local. addSubstitution local (SInt n)
	>>| return ([], local)

genExpr (Nested loc e) m = genExpr e m

genExpr (PlusExpr loc l r) m = genExpr l m
	>>= \(lcode, lid). genExpr r m
	>>= \(rcode, rid). newLocal
	>>= \resultLocal. getSubstitution lid
	>>= \lsub. getSubstitution rid
	>>= \rsub. return (lcode ++ rcode ++ ["%" + resultLocal + " = " + "add i64 " + substitutionToConstant lsub + ", " + substitutionToConstant rsub], resultLocal)

genExpr (MinusExpr loc l r) m = genExpr l m
	>>= \(lcode, lid). genExpr r m
	>>= \(rcode, rid). newLocal
	>>= \resultLocal. getSubstitution lid
	>>= \lsub. getSubstitution rid
	>>= \rsub. return (lcode ++ rcode ++ ["%" + resultLocal + " = " + "sub i64 " + substitutionToConstant lsub + ", " + substitutionToConstant rsub], resultLocal)

genExpr (TimesExpr loc l r) m = genExpr l m
	>>= \(lcode, lid). genExpr r m
	>>= \(rcode, rid). newLocal
	>>= \resultLocal. getSubstitution lid
	>>= \lsub. getSubstitution rid
	>>= \rsub. return (lcode ++ rcode ++ ["%" + resultLocal + " = " + "mul i64 " + substitutionToConstant lsub + ", " + substitutionToConstant rsub], resultLocal)

genExpr (EqExpr loc l r) m = genExpr l m
	>>= \(lcode, lresult). genExpr r m
	>>= \(rcode, rresult). newLocal
	>>= \resultLocal. getSubstitution lresult
	>>= \lsub. getSubstitution rresult
	>>= \rsub. return (lcode ++ rcode ++ ["%" + resultLocal + " = " + "icmp eq i64 " + substitutionToConstant lsub + ", " + substitutionToConstant rsub], resultLocal)

genExpr (FuncExpr loc name args) m = case get name m of
	Nothing = error ("No type for function " + name)
	Just (_, ftype, True) = return ([], name)
	Just (_, ftype, False)
	# argTypes = map toLLVMType (withoutReturnType ftype)
	# rettype = returnType ftype
	# fcall = "tail call ccc " + toLLVMType rettype + " @" + name
	= mapM (\a. genExpr a m) args
		>>= \argsCode. newLocal
		>>= \resultLocal. mapM retrieveArguments (zip2 argTypes argsCode)
		>>= \argsWithSubs. return (retrieveCode argsCode ++ ["%" + resultLocal + " = " + fcall + "(" + join ", " argsWithSubs + ")"], resultLocal)
where
	retrieveCode [] = []
	retrieveCode [(c, _) : cs] = c ++ retrieveCode cs

	retrieveArguments (t, (_, l)) = getSubstitution l
		>>= \sub. return (t + " " + substitutionToConstant sub)


genExpr e m = error ("genCode not matching: " + toString e)

