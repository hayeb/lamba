definition module Lamba.CodeGenerator

import Data.Error
import Lamba.Language.AST

from Data.Map import :: Map

:: CodeGenError :== String
:: Label :== String

generateCode :: AST (Map String (SourceLocation, Type)) -> MaybeError CodeGenError [String]

