module lambac

import System.IO, System.File
import StdEnv, ArgEnv
import Data.Error
from Control.Monad import class Monad(..)
import Control.Applicative
import Text

import Lamba

:: *CompilerState = { fileName :: String
				   , fileContents :: Maybe String
				   , errors :: [String]
				   , world :: !*World
				   , console :: !*File
				   }

:: CompilerIO a = CIO (*CompilerState -> *(MaybeError String a, *CompilerState))

instance Functor CompilerIO
where
	fmap f (CIO g) = CIO \cs. case g cs of
		(Error e, cs) = (Error e, cs)
		(Ok a, cs) = (Ok (f a), cs)

instance Applicative CompilerIO
where
	pure a = CIO \cs. (Ok a, cs)
	(<*>) (CIO f) (CIO a) = CIO \cs. case f cs of
		(Error e, cs) = (Error e, cs)
		(Ok h, cs) = case a cs of
			(Error e, cs) = (Error e, cs)
			(Ok a, cs) = (Ok (h a), cs)

instance Monad CompilerIO
where
	bind (CIO a) fCIOB = CIO \cs. case a cs of
		(Error e, cs) = (Error e, cs)
		(Ok val, cs)
		# (CIO f)= fCIOB val
		= case f cs of
			(Error e, cs) = (Error e, cs)
			(Ok v, cs) = (Ok v,cs)

compileError s = print s
	>>| CIO \cs. (Error s, cs)

getFileName = CIO \cs=:{fileName}. (Ok fileName, cs)
getFileContents = CIO \cs=:{fileContents}. case fileContents of
	Nothing = (Error "No file contents!", cs)
	Just f = (Ok f, cs)

print :: String -> CompilerIO ()
print msg = CIO \cs=:{console}.
	(Ok (), {cs & console = fwrites (msg + "\n") console})

openFile :: CompilerIO String
openFile = CIO \cs. case readFile cs.fileName cs.world of
	(Error e, world) = (Error (toString e), {cs & world = world})
	(Ok contents, world) = (Ok contents, {cs & fileContents = Just contents, world = world})

formatTokens :: [(TokenLocation, Token)] -> String
formatTokens tokens = join "\n" (map toString tokens)

initState filename world 
# (console, world) = stdio world
= { fileName = filename
  , fileContents = Nothing
  , errors = []
  , world = world
  , console = console}

Start env = case main env of
	(Error e, env) = env
	(Ok s, env) = env
where
	main :: *World -> (MaybeError String (), *World)
	main world
	# args = getCommandLine
	| size args == 1 = (Error "Usage: lambac <FILENAME>", world)
	# (CIO f) = compile
	= case f (initState args.[1] world) of
		(Error e, {world}) = (Error e, world)
		(Ok result, {world}) = (Ok result, world)

	compile = openFile
		>>= \contents. case tokenize contents of
			Left e = compileError (toString e)
			Right tokens = pure tokens
		>>= \tokens. case parse tokens of
			Left e = compileError (toString e)
			Right ast = pure ast
		>>= \ast. print ("Parsing succeeded. AST: \n" + toString ast)
		>>| return ()
