module TypeInference

import Lamba
import Text
from Data.Map import :: Map, newMap, fromList
import Data.Error
import StdMisc, StdDebug

emptyState :: IEnv
emptyState = {fresh = 0, types = newMap}

simpleExprs :: [(String, IEnv, (MaybeError [InferenceError] [Substitution], IEnv), Infer [Substitution])]
simpleExprs = [ ("SimpleBoolean",
					emptyState,
					(Ok [], emptyState),
					algM (BoolExpr (0,0) True) TBool)
			  , ("SimpleNumber",
			  		emptyState,
			  		(Ok [], emptyState),
					algM (NumberExpr (0,0) 18) TInt)
			  , ("SimpleString",
			  		emptyState,
			  		(Ok [], emptyState),
					algM (StringExpr (0,0) "18") TString)
			  , ("SimpleChar",
			  		emptyState,
			  		(Ok [], emptyState),
					algM (CharExpr (0,0) 'l') TChar)
			  ]

nestedExprs :: [(String, IEnv, (MaybeError [InferenceError] [Substitution], IEnv), Infer [Substitution])]
nestedExprs = [("Nested",
	emptyState,
	(Ok [], emptyState),
	algM (Nested (0,0) (BoolExpr (0,2) True)) TBool)]

tupleExprs :: [(String, IEnv, (MaybeError [InferenceError] [Substitution], IEnv), Infer [Substitution])]
tupleExprs
= [("Tuple",
		emptyState,
   		(Ok [(0, TBool), (1, TBool)], {emptyState & fresh = 2}),
   		algM (TupleExpr (0,0) [(BoolExpr (0, 1) True), (BoolExpr (0, 2) False)]) (TTuple [TBool, TBool]))
   , ("TupleWrongArityLess",
   		emptyState,
   		(Error [InferenceError (0,3) (toString (ArityError 2 3))], {emptyState & fresh = 3}),
   		algM (TupleExpr (0,3) [(BoolExpr (0, 4) True), (BoolExpr (0, 5) False), (BoolExpr (0, 6) True)])
			(TTuple [TBool, TBool]))
   , ("TupleWrongArityMore",
   		emptyState,
   		(Error [InferenceError (0, 7) (toString (ArityError 3 2))], {emptyState & fresh = 2}),
   		algM (TupleExpr (0, 7) [(BoolExpr (0, 8) True), (BoolExpr (0, 9) False)])
			(TTuple [TBool, TBool, TBool]))
   , ("TupleVarType",
		emptyState,
   		(Ok [(0, TBool), (1, TBool), (-1, (TTuple [TBool, TBool]))], {emptyState & fresh = 2}),
   		algM (TupleExpr (0,0) [(BoolExpr (0, 1) True), (BoolExpr (0, 2) False)]) (TVar -1))
   , ("TupleVarLeft",
   		emptyState,
   		(Ok [(0, TBool), (1, TBool), (-1, TBool)], {emptyState & fresh = 2}),
   		algM (TupleExpr (0,0) [(BoolExpr (0, 1) True), (BoolExpr (0, 2) False)]) (TTuple [TVar -1, TBool]))
   , ("TupleVarRight",
   		emptyState,
   		(Ok [(0, TBool), (1, TBool), (-1, TBool)], {emptyState & fresh = 2}),
   		algM (TupleExpr (0,0) [(BoolExpr (0, 1) True), (BoolExpr (0, 2) False)]) (TTuple [TBool, TVar -1]))
	, ("TupleErrorLeft",
		emptyState,
   		(Error [InferenceError (0,0) (toString (UnificationError TInt TBool))], {emptyState & fresh = 2}),
   		algM (TupleExpr (0,0) [(BoolExpr (0, 1) True), (BoolExpr (0, 2) False)]) (TTuple [TInt, TVar -1]))
	, ("TupleErrorBoth",
		emptyState,
   		(Error [InferenceError (0,0) (toString (UnificationError TInt TBool))
			   , InferenceError (0,0) (toString (UnificationError TInt TBool))]
			, {emptyState & fresh = 2}),
   		algM (TupleExpr (0,0) [(BoolExpr (0, 1) True), (BoolExpr (0, 2) False)]) (TTuple [TInt, TInt]))
  ]

listExprs
= [("SimpleList",
		emptyState,
   		(Ok [(0, TInt), (1, TInt)], {emptyState & fresh = 2}),
   		algM (ListExpr (0,0) (NumberExpr (0,1) 1) (EmptyList (0,2))) (TList TInt))
	, ("EmptyListInt",
		emptyState,
   		(Ok [(0, TInt)], {emptyState & fresh = 1}),
   		algM (EmptyList (0,0)) (TList TInt))
	, ("EmptyListBool",
		emptyState,
   		(Ok [(0, TBool)], {emptyState & fresh = 1}),
   		algM (EmptyList (0,0)) (TList TBool))
	, ("EmptyListChar",
		emptyState,
   		(Ok [(0, TChar)], {emptyState & fresh = 1}),
   		algM (EmptyList (0,0)) (TList TChar))
	, ("EmptyListString",
		emptyState,
   		(Ok [(0, TString)], {emptyState & fresh = 1}),
   		algM (EmptyList (0,0)) (TList TString))
  ]

funcExprs
= [("Variable",
		{emptyState & types = fromList [("test", ((0,1), TBool))]},
		(Ok [(-1, TBool)], {emptyState & fresh = 0, types = fromList [("test", ((0,1), TBool))]}),
		algM (FuncExpr (0,0) "test" []) (TVar -1)),
	("VariableUndefined",
		emptyState,
		(Error [UndefinedVariableError "test" (0,0)], emptyState),
		algM (FuncExpr (0,0) "test" []) (TVar -1)),
	("FunctionArity1",
		{emptyState & types = fromList [("test", ((0,0), TFunc TBool TBool))]},
		(Ok [], {emptyState & fresh = 2, types = fromList [("test", ((0,0), TFunc TBool TBool))]}),
		algM (FuncExpr (0,0) "test" [BoolExpr (0,1) True]) TBool),
	("FunctionArity1WrongArgument",
		{emptyState & types = fromList [("test", ((0,0), TFunc TBool TBool))]},
		(Error [InferenceError (0,0) (toString (FunctionApplicationError "test" (TFunc TInt TBool) (TFunc TBool TBool)))],
			{emptyState & fresh = 2, types = fromList [("test", ((0,0), TFunc TBool TBool))]}),
		algM (FuncExpr (0,0) "test" [NumberExpr (0,1) 1]) TBool),
	("FunctionTooManyArguments",
		{emptyState & types = fromList [("test", ((0,0), TFunc TBool TBool))]},
		(Error [InferenceError (0,0) (toString (FunctionApplicationError "test" (TFunc TInt (TFunc TInt TBool)) (TFunc TBool TBool)))],
			{emptyState & fresh = 3, types = fromList [("test", ((0,0), TFunc TBool TBool))]}),
		algM (FuncExpr (0,0) "test" [NumberExpr (0,1) 1, NumberExpr (0,2) 2]) TBool),
	("FunctionTooFewArguments",
		{emptyState & types = fromList [("test", ((0,0), TFunc TBool (TFunc TBool TBool)))]},
		(Error [InferenceError (0,0) (toString (FunctionApplicationError "test" (TFunc TBool TBool) (TFunc TBool (TFunc TBool TBool))))],
			{emptyState & fresh = 2, types = fromList [("test", ((0,0), TFunc TBool (TFunc TBool TBool)))]}),
		algM (FuncExpr (0,0) "test" [BoolExpr (0,1) True]) TBool)
  ]

Start = join "\n" (executeTests (simpleExprs
	++ nestedExprs
	++ tupleExprs
	++ listExprs
	++ funcExprs))

executeTests :: [(String, IEnv, (MaybeError [InferenceError] [Substitution], IEnv), Infer [Substitution])] -> [String]
executeTests [] = []
executeTests [(name, env, expected, test) : rest]
# result = runInfer test env
| result ==  expected = [name + " passed." : executeTests rest]
= [name + " failed.\n\tGot: " + toString result + "\n\tExpected: " + toString expected : executeTests rest]

instance toString (MaybeError [InferenceError] [Substitution])
where
	toString (Ok []) = "Ok []"
	toString (Ok a) = "Ok [" + join ", " (map toString a) + "]"
	toString (Error []) = "Error []"
	toString (Error e) = "Error [" + join ", " (map toString e) + "]"
