implementation module Lamba.CodeGenerator

import Lamba.Language.AST
import Data.Map, Data.Error
from StdList import ++

class genCode a where
	genCode :: a Label (Map SourceLocation Type) -> MaybeError [CodeGenError] [AssemblyInstruction]

(-&-) infixl 1 :: (MaybeError [e] [a]) (MaybeError [e] [a]) -> MaybeError [e] [a]
(-&-) l r = case (l, r) of
	(Ok i1, Ok i2) = Ok (i1 ++ i2)
	(Error es, Ok is) = Error es
	(Ok is, Error es) = Error es
	(Error es1, Error es2) = Error (es1 ++ es2)

insertLabel label [AssemblyInstruction Nothing ins c: is] = [AssemblyInstruction (Just label) ins c : is]
insertLabel label [ains=:(AssemblyInstruction (Just _) ins c): is] = [AssemblyInstruction (Just label) NOP Nothing : [ains : is]]

generateCode :: AST (Map SourceLocation Type) -> MaybeError [CodeGenError] [AssemblyInstruction]
generateCode ast types = genCode ast "" types

instance genCode AST
where
	genCode (AST []) _ _ = Ok []
	genCode (AST [d:ds]) label m = genCode d label m -&- genCode (AST ds) label m

instance genCode FDecl
where
	genCode (FDecl _ _ _ []) label m = Ok []
	genCode (FDecl loc name type [b:bs]) label m = genCode b ("d_"+name) m -&- genCode (FDecl loc name type bs) label m

instance genCode FBody
where
	genCode (FBody loc name matches guards) label m
	# label = label + "_b_" + toString loc
	= Ok []

instance genCode Match
where
	genCode (MVar _) = Ok []


instance genCode Expr
where
	genCode
