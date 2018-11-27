module TypeInference

import Lamba
import Text
from Data.Map import :: Map, newMap
import Data.Error

emptyState :: IEnv
emptyState = {fresh = 0, types = newMap}

simpleExprs :: [(String, (MaybeError [InferenceError] [Substitution], IEnv), Infer [Substitution])]
simpleExprs = [ ("SimpleBoolean", (Ok [], emptyState), algM (BoolExpr (0,0) True) TBool)
			  , ("SimpleNumber", (Ok [], emptyState), algM (NumberExpr (0,0) 18) TInt)
			  , ("SimpleString", (Ok [], emptyState), algM (StringExpr (0,0) "18") TString)
			  , ("SimpleChar", (Ok [], emptyState), algM (CharExpr (0,0) 'l') TChar)
			  ]

nestedExprs :: [(String, (MaybeError [InferenceError] [Substitution], IEnv), Infer [Substitution])]
nestedExprs = [("Nested", (Ok [], emptyState), algM (Nested (0,0) (BoolExpr (0,2) True)) TBool)]

tupleExprs :: [(String, (MaybeError [InferenceError] [Substitution], IEnv), Infer [Substitution])]
tupleExprs
= [("SimpleTuple",
   		(Ok [(0, TBool), (1, TBool)], {emptyState & fresh = 2}),
   		algM (TupleExpr (0,0) [(BoolExpr (0, 1) True), (BoolExpr (0, 2) False)]) (TTuple [TBool, TBool]))
   , ("WrongArityLess",
   		(Error [InferenceError (0,3) (toString (ArityError 2 3))], {emptyState & fresh = 3}),
   		algM (TupleExpr (0,3) [(BoolExpr (0, 4) True), (BoolExpr (0, 5) False), (BoolExpr (0, 6) True)])
			(TTuple [TBool, TBool]))
   , ("WrongArityMore",
   		(Error [InferenceError (0, 7) (toString (ArityError 3 2))], {emptyState & fresh = 2}),
   		algM (TupleExpr (0, 7) [(BoolExpr (0, 8) True), (BoolExpr (0, 9) False)])
			(TTuple [TBool, TBool, TBool]))
   , ("SimpleTupleVarType",
   		(Ok [(0, TBool), (1, TBool)], {emptyState & fresh = 2}),
   		algM (TupleExpr (0,0) [(BoolExpr (0, 1) True), (BoolExpr (0, 2) False)]) (TVar 0))
  ]

Start = join "\n" (executeTests (simpleExprs ++ nestedExprs ++ tupleExprs))

executeTests :: [(String, (MaybeError [InferenceError] [Substitution], IEnv), Infer [Substitution])] -> [String]
executeTests [] = []
executeTests [(name, expected, test) : rest]
# result = runInfer test emptyState
| result ==  expected = [name + " passed." : executeTests rest]
= [name + " failed.\nGot: " + toString result + "\nExpected: " + toString expected : executeTests rest]

instance toString (MaybeError [InferenceError] [Substitution])
where
	toString (Ok []) = "Ok []"
	toString (Ok a) = "Ok [" + join ", " (map toString a) + "]"
	toString (Error []) = "Error []"
	toString (Error e) = "Error [" + join ", " (map toString e) + "]"
