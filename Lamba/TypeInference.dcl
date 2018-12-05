definition module Lamba.TypeInference

import Data.Error
import Lamba.Language.AST

from Data.Map import :: Map

:: TypeScope :== Map String (SourceLocation, Type)

// Type inference environment
:: IEnv = { fresh :: Int
		  , types :: TypeScope
		  , exprTypes :: Map SourceLocation Type
		  }

// A substitution is replacing all references to the type variable with the
// second element of the tuple.
:: Substitution :== (Int, Type)

:: Infer a = Infer (IEnv -> (MaybeError [InferenceError] a, IEnv))

:: UnificationError = UnificationError Type Type // Could not unify t1 with t2
	| ArityError Int Int // Tuple of function with wrong arity
	| FunctionApplicationError String Type Type // Name, derived type, demanded type

:: InferenceError = InferenceError SourceLocation String
	| UndefinedVariableError String SourceLocation

infer :: AST -> MaybeError [InferenceError] (Map SourceLocation Type)
runInfer :: (Infer a) IEnv -> (MaybeError [InferenceError] a, IEnv)

instance toString IEnv, InferenceError, UnificationError
instance == IEnv, InferenceError

class algM a
where
	algM :: a Type -> Infer [Substitution]

instance algM Expr

instance algM FBody
