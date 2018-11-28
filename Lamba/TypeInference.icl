implementation module Lamba.TypeInference

import Control.Applicative
import Data.Error, Data.Map, Data.Tuple, Data.Functor, Data.GenEq
import Lamba.Language.AST
import StdInt, StdMisc, StdDebug
import Text

from Control.Monad import class Monad(..)
from StdList import map, ++, instance length [], foldl

instance toString UnificationError
where
	toString (UnificationError l r) = "Could not unify \n\t" + toString l + "\nwith\n\t" + toString r
	toString  (ArityError expected got) = "Wrong arity. Expected: " + toString expected + ". Got: " + toString got

instance toString InferenceError
where
	toString (InferenceError (line, col) str)
	= "[" + toString line + ":" + toString col + "] Type error: " + str

instance toString IEnv
where
	toString {fresh, types} = "IEnv "
		+ toString fresh + (case null types of
				True = " []"
				False = join "\n" (map (\(loc, (name, type)). toString loc + " " + name + " :: " + toString type) (toList types)))

derive gEq IEnv, InferenceError, Type
instance == IEnv
where
	(==) e1 e2 = e1 === e2

instance == InferenceError
where
	(==) e1 e2 = e1 === e2

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

(-&-) infixl 3 :: (MaybeError [e] [a]) (MaybeError [e] [a]) -> MaybeError [e] [a]
(-&-) (Ok la) (Ok ra) = Ok (la ++ ra)
(-&-) (Error le) (Ok ra) = Error le
(-&-) (Ok la) (Error re) = Error re
(-&-) (Error le) (Error re) = Error (le ++ re)

freshFunction :: String SourceLocation -> Infer Type
freshFunction fname loc = Infer \state=:{fresh, types}.
	(Ok (TVar fresh), {state & fresh = inc fresh
								 , types = put loc (fname, TVar fresh) types})

fresh :: Infer Type
fresh = Infer \state=:{fresh}. (Ok (TVar fresh), {state & fresh = inc fresh})

freshN n = Infer \state=:{fresh}. (Ok [TVar n \\ n <- [fresh..fresh + n - 1]], {state & fresh = fresh + n})

applyEnv :: [Substitution] -> Infer ()
applyEnv subs = Infer \state=:{types}. (Ok (), applySubstitutionsEnv subs state)

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
unify (TVar l) (TVar r) = Ok [(l, (TVar r))]
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
		>>= \res. algM (AST ds) t
		>>= \res`. return (res ++ res`)

instance algM FDecl
where
	algM (FDecl loc name Nothing bodies) _
	// Record that we have encountered a function without type in the environment
	= return []

	algM (FDecl loc name (Just type) bodies) _
	= return []

instance algM Expr
where
	algM (NumberExpr loc _) t = liftUnify loc t TInt
	algM (StringExpr loc _) t = liftUnify loc t TString
	algM (CharExpr loc _)   t = liftUnify loc t TChar
	algM (BoolExpr loc _)   t = liftUnify loc t TBool

	algM (Nested loc e) t = algM e t

	// TODO: This is not correct.
	algM (TupleExpr loc els) t = freshN (length els)
		>>= \elementVars. liftUnify loc t (TTuple elementVars)

	algM (ListExpr loc h t) type

	= fresh
		>>= \headVar. algM h headVar
		>>= \subs. algM t (applySubstitutions subs (TList headVar))
		>>= \subs`. let allsubs = subs ++ subs` in
			return (applySubstitutions allsubs type, applySubstitutions allsubs headVar)
		>>= \(reqT, hT). liftUnify loc reqT (TList hT)

	algM (EmptyList loc) type = fresh
		>>= \tt. liftUnify loc type tt

