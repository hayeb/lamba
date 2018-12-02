implementation module Lamba.TypeInference

import Control.Applicative
import Data.Error, Data.Map, Data.Tuple, Data.Functor, Data.GenEq
import Lamba.Language.AST
import Lamba.Formatter
import StdInt, StdMisc, StdBool, StdTuple, StdDebug
import Text

from Control.Monad import class Monad(..), mapM
from StdList import map, ++, instance length [], foldl, zip2, flatten

import qualified Data.List as DL

instance toString UnificationError
where
	toString (UnificationError l r) = "Could not unify \n\t" + toString l + "\nwith\n\t" + toString r
	toString (ArityError expected got) = "Wrong arity. Expected: " + toString expected + ". Got: " + toString got
	toString (FunctionApplicationError name derived demanded) = "Function \"" + name + "\" has derived type\n\t" + formatType derived + "\nwhile demanded\n\t" + formatType demanded

instance toString InferenceError
where
	toString (InferenceError (line, col) str)
	= "[" + toString line + ":" + toString col + "] Type error: " + str

	toString (UndefinedVariableError var loc) = toString loc + "Undefined variable: " + var

instance toString IEnv
where
	toString {fresh, types} = "IEnv "
		+ toString fresh + (case null types of
				True = " []"
				False = join "\n" (map (\(name, (loc, type)). toString loc + " " + name + " :: " + toString type) (toList types)))

derive gEq IEnv, InferenceError, Type
instance == IEnv
where
	(==) e1 e2 = e1 === e2

instance == InferenceError
where
	(==) e1 e2 = e1 === e2

instance == Substitution
where
	(==) (v1, t1) (v2, t2) = v1 == v2 && t1 == t2

instance Functor Infer
where
	fmap f (Infer a) = Infer \state. appFst (fmap f) (a state)

instance pure Infer
where
	pure v = Infer \state. (Ok v, state)

instance <*> Infer
where
	(<*>) (Infer l) (Infer r) = Infer \state. case l state of
		(Error e, state) = (Error e, state)
		(Ok ll, state) = case r state of
			(Error e, state) = (Error e, state)
			(Ok rr, state) = (Ok (ll rr), state)

instance Monad Infer
where
	bind (Infer a) fa = Infer \state. case a state of
		(Error e, state) = (Error e, state)
		(Ok aa, state) = case fa aa of
			(Infer ffa) = ffa state

runInfer :: (Infer a) IEnv -> (MaybeError [InferenceError] a, IEnv)
runInfer (Infer f) state = f state

error :: String SourceLocation -> Infer a
error err location = Infer \state. (Error [InferenceError location err], state)

(-&-) infixl 3 :: (MaybeError [e] [a]) (MaybeError [e] [a]) -> MaybeError [e] [a]
(-&-) (Ok la) (Ok ra) = Ok (la ++ ra)
(-&-) (Error le) (Ok ra) = Error le
(-&-) (Ok la) (Error re) = Error re
(-&-) (Error le) (Error re) = Error (le ++ re)

freshFunction :: String SourceLocation -> Infer Type
freshFunction fname loc = Infer \state=:{fresh, types}.
	(Ok (TVar fresh), {state & fresh = inc fresh
								 , types = put fname (loc, TVar fresh) types})

storeFunctionType :: String SourceLocation Type -> Infer Type
storeFunctionType fname loc type = Infer \state=:{types}.
	(Ok type, {state & types = put fname (loc, type) types})

fresh :: Infer Type
fresh = Infer \state=:{fresh}. (Ok (TVar fresh), {state & fresh = inc fresh})

freshN n = Infer \state=:{fresh}. (Ok [TVar n \\ n <- [fresh..fresh + n - 1]], {state & fresh = fresh + n})

applyEnv :: [Substitution] -> Infer ()
applyEnv subs = Infer \state=:{types}. (Ok (), applySubstitutionsEnv subs state)

retrieve :: SourceLocation String -> Infer Type
retrieve loc name = Infer \state=:{types}.
	case get name types of
	Nothing = (Error [UndefinedVariableError name loc], state)
	Just type = (Ok (snd type), state)

infer :: AST -> MaybeError [InferenceError] TypeScope
infer ast
# (Infer infer) = algM ast TVoid
# initialState = {fresh = 0, types = newMap}
= case infer initialState of
	(Error es, st) = Error es
	(Ok subs, {types}) = Ok types

applySubstitutionsEnv :: [Substitution] IEnv -> IEnv
applySubstitutionsEnv subs env=:{types}
= {env & types = applyScope subs types}
where
	applyScope :: [Substitution] TypeScope -> TypeScope
	applyScope subs scope
	# scopeList = toList scope
	# applied = map (\(loc, (tname, ttype)). (loc, (tname, applySubstitutions subs ttype))) scopeList
	= fromList applied

applySubstitutions :: [Substitution] Type -> Type
applySubstitutions [] t = t
applySubstitutions [s : ss] t
# t = applySubstitution s t
= applySubstitutions ss t
where
	applySubstitution (sname, stype) (TVar name)
	| sname == name = stype
	= TVar name

	applySubstitution sub (TTuple types) = TTuple (map (applySubstitution sub) types)
	applySubstitution sub (TList type) = TList (applySubstitution sub type)
	applySubstitution sub (TFunc fromt tot) = TFunc (applySubstitution sub fromt) (applySubstitution sub tot)
	applySubstitution _ type = type

liftUnify :: SourceLocation Type Type -> Infer [Substitution]
liftUnify loc t1 t2 = Infer \state. case unify t1 t2 of
	Error errors = (Error (map (\e. InferenceError loc (toString e)) errors), state)
	Ok subs = (Ok subs, state)

unify :: Type Type -> MaybeError [UnificationError] [Substitution]
unify (TVar l) (TVar r) = Ok (if (l == r) [] [(l, (TVar r))])
unify (TVar l) t = Ok [(l, t)]
unify t (TVar r) = Ok [(r, t)]

unify (TTuple []) (TTuple []) = Ok []

unify (TTuple l) (TTuple r)
| length l <> length r = Error [ArityError (length l) (length r)]
= unifyTuple (TTuple l) (TTuple r)
where
	unifyTuple (TTuple []) (TTuple []) = Ok []
	unifyTuple (TTuple [l:ls]) (TTuple [r:rs]) = unify l r -&- unifyTuple (TTuple ls) (TTuple rs)

unify (TList l) (TList r) = unify l r
unify (TFunc fl tl) (TFunc fr tr) = unify fl fr -&- unify tl tr

unify TBool TBool = Ok []
unify TInt TInt = Ok []
unify TChar TChar = Ok []
unify TVoid TVoid = Ok []
unify TString TString = Ok []
unify l r = Error [UnificationError l r]

instance algM AST
where
	algM (AST []) _ = pure []
	algM (AST [d:ds]) t = algM d t
		>>= \res. applyEnv res
		>>| algM (AST ds) t
		>>= \res`. return ('DL'.union res res`)

instance algM FDecl
where
	algM (FDecl loc name Nothing bodies) _
	// Record that we have encountered a function without type in the environment
	= freshFunction name loc
		>>| mapM (\b. algM b TVoid) bodies
		>>= \subs. return (flatten subs)

	algM (FDecl loc name (Just type) bodies) _
	= storeFunctionType name loc type
		>>| mapM (\b. algM b TVoid) bodies
		>>= \subs. return (flatten subs)

instance algM FBody
where
	/* algM function body
	 * 1. Check arity of the matches
	 */
	algM (FBody loc name matches guards) _
	= retrieve loc name
		>>= \ftype. checkArity ftype matches
		>>| freshN (length matches)
		>>= \matchVariables. mapM (\(var, match). algM match var) (zip2 matchVariables matches)
		>>= \subs1. return []
	where
		// If we encounter a function body for which we do not know the exact type, we cannot know whether the arity is
		// correct yet. Assume it is for now, errors will come when considering different alternatives of the same function
		// or different applications of the function.
		checkArity (TVar _) _ = return ()
		checkArity ftype matches
		# functionArity = arity ftype
		| arity ftype <> length matches = error ("Function body has "
			+ toString (length matches)
			+ " arguments, while type requires "
			+ toString (arity ftype)
			+ " arguments.") loc
		= return ()

instance algM Match
where
	algM _ _ = return []

instance algM Expr
where
	algM (NumberExpr loc _) t = liftUnify loc t TInt
	algM (StringExpr loc _) t = liftUnify loc t TString
	algM (CharExpr loc _)   t = liftUnify loc t TChar
	algM (BoolExpr loc _)   t = liftUnify loc t TBool
	algM (Nested loc e) t 	  = algM e t

	algM (TupleExpr loc els) t
	= freshN (length els)
		>>= \fresh. mapM (\(tvar, e). algM e tvar) (zip2 fresh els)
		>>= \substitutions. let subs = flatten substitutions in
			liftUnify loc (applySubstitutions subs t) (TTuple (map (applySubstitutions subs) fresh))
		>>= \unifySubs. return ('DL'.union subs unifySubs)

	algM (ListExpr loc h t) type
	= fresh
		>>= \headVar. algM h headVar
		>>= \headSubs. algM t (applySubstitutions headSubs (TList headVar))
		>>= \tailSubs. let allSubs = 'DL'.union headSubs tailSubs in
			return (applySubstitutions allSubs type, applySubstitutions allSubs headVar)
		>>= \(reqT, hT). liftUnify loc reqT (TList hT)
		>>= \unifySubs. return ('DL'.union allSubs unifySubs)

	algM (EmptyList loc) type = fresh
		>>= \tt. liftUnify loc type (TList tt)

	algM (FuncExpr loc name []) type
	= retrieve loc name
		>>= \functionType. liftUnify loc type functionType

	/* algM function:
	 * 1. Retrieve function type, error if undefiend
	 * 2. Generate fresh variables for each argument expression
	 * 3. Run algM on each of the arguments.
	 * 4. Apply the substitutions to the function type and the demanded type
	 * 5. Transform the substitutions into a function type
	 * 6. Unify the function type with the demanded type
	 */
	algM (FuncExpr loc name arguments) type
	= retrieve loc name // Gives error if function undefined
		>>= \functionType. freshN (length arguments)
		>>= \vars. mapM (\(tvar, e). algM e tvar) (zip2 vars arguments)
		>>= \sub1. fresh
		>>= \retVar. liftUnify loc retVar type
		>>= \sub2. (let subs = flatten sub1 ++ sub2 in
				   let fType = toFunctionType (map (applySubstitutions subs) vars ++ [applySubstitutions subs retVar]) in
				   liftUnifyFunc name loc fType functionType)
	where
		toFunctionType :: [Type] -> Type
		toFunctionType [f, t] = TFunc f t
		toFunctionType [e:es] = TFunc e (toFunctionType es)

		returnType :: Type -> Type
		returnType (TFunc f t) = returnType t
		returnType t = t

		liftUnifyFunc :: String SourceLocation Type Type -> Infer [Substitution]
		liftUnifyFunc name loc derived required = Infer \state. case unify derived required of
			Error _ = (Error [InferenceError loc (toString (FunctionApplicationError name derived required))], state)
			Ok subs = (Ok subs, state)

