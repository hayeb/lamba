definition module Lamba.CodeGenerator

import Data.Error
import Lamba.Language.AST

from Data.Map import :: Map

:: CodeGenError :== String
:: Label :== String
:: Comment :== String

:: Instruction = NOP

:: AssemblyInstruction = AssemblyInstruction (Maybe Label) Instruction (Maybe Comment)

class genCode a where
	genCode :: a (Map SourceLocation Type) -> MaybeError [CodeGenError] [AssemblyInstruction]

instance genCode AST

