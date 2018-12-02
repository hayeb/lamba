module lambac

import System.File
import StdEnv, ArgEnv
import Data.Error
from Control.Monad import class Monad(..)
import Control.Applicative
import Text

import Lamba

from Data.Map import toList, toAscList, foldrWithKey

:: *CompilerState = { fileName :: String
				    , fileContents :: Maybe String
				    , world :: !*World
				    , console :: !*File
				    }

// We use uniqueness to guarantee we never pass the wrong state.
:: CompilerIO a = CIO (*CompilerState -> *(MaybeError String a, *CompilerState))

instance Functor CompilerIO
where
	fmap f (CIO g) = CIO \cs. case g cs of
		(Error e, cs) = (Error e, cs)
		(Ok a, cs) = (Ok (f a), cs)

instance pure CompilerIO
where
	pure a = CIO \cs. (Ok a, cs)

instance <*> CompilerIO
where
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
		= f cs

compileError s = print s
	>>| CIO \cs. (Error s, cs)

compileInfo s v = print s
	>>| CIO \cs. (Ok v, cs)

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

formatTypesEnv :: (Map SourceLocation (String, Type)) -> String
formatTypesEnv types = join "\n" (map (\((line, col), (name, type)). "["
	+ toString line
	+ ":"
	+ toString col
	+ "] "
	+ name
	+ ": "
	+ toString type)
	(toList types))

initState filename world
# (console, world) = stdio world
= { fileName = filename
  , fileContents = Nothing
  , world = world
  , console = console}

Start env = main env
where
	main :: *World -> *World
	main world
	# args = getCommandLine
	| size args == 1
		# (console, world) = stdio world
		# console = fwrites "Usage: lambac <FILENAME>\n" console
		= closeConsole console world
	# (CIO f) = compile
	= case f (initState args.[1] world) of
		(_, {console, world}) = closeConsole console world

	compile = openFile
		>>= \contents. case tokenize contents of
			Error e = compileError (toString e)
			Ok tokens = compileInfo ("Tokens: " + formatTokens tokens) tokens
		>>= \tokens. case parse tokens of
			Error e = compileError (toString e)
			Ok ast = pure ast
		>>= \ast. print ("Parsing succeeded. AST: \n" + toString ast)
		>>| case infer ast of
			Error e = compileError (join "\n" (map toString e))
			Ok types = print ("Type inferencing succeeded")
		>>| return ()

	closeConsole console world
	# (_, world) = fclose console world
	= world
