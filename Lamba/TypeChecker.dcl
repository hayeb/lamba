definition module Lamba.TypeChecker

import Data.Error

import Lamba.Language.AST

:: TEnv :== [(String, SourceLocation, Type)]
:: TypeError = TypeError SourceLocation String

instance toString TypeError

typecheck :: AST -> MaybeError [TypeError] TEnv 
