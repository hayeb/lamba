definition module Lamba.CodeGenerator

import Data.Error
import Lamba.Language.AST

from Data.Map import :: Map

:: CodeGenError :== String
:: Label :== String
:: Comment :== String

:: Instruction = NOP

:: AssemblyInstruction = AssemblyInstruction (Maybe Label) Instruction (Maybe Comment)

generateCode :: AST (Map SourceLocation Type) -> MaybeError [CodeGenError] [AssemblyInstruction]

