implementation module Lamba.CodeGenerator

import Lamba.Language.AST
import Data.Map, Data.Error
from StdList import ++

(-&-) infixl 1 :: (MaybeError [e] [a]) (MaybeError [e] [a]) -> MaybeError [e] [a]
(-&-) l r = case (l, r) of
	(Ok i1, Ok i2) = Ok (i1 ++ i2)
	(Error es, Ok is) = Error es
	(Ok is, Error es) = Error es
	(Error es1, Error es2) = Error (es1 ++ es2)

insertLabel label [AssemblyInstruction Nothing ins c: is] = [AssemblyInstruction (Just label) ins c : is]
insertLabel label [ains=:(AssemblyInstruction (Just _) ins c): is] = [AssemblyInstruction (Just label) NOP Nothing : [ains : is]]

instance genCode AST
where
	genCode (AST []) _ = Ok []
	genCode (AST [d:ds]) m = genCode d m -&- genCode (AST ds) m

instance genCode FDecl
where
	genCode (FDecl _ _ _ []) m = Ok []
	genCode (FDecl loc name type [b:bs]) m = genCode b m -&- genCode (FDecl loc name type bs) m

instance genCode FBody
where
	genCode (FBody loc name matches guards) m = Ok []



