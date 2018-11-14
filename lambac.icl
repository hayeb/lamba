module lambac

import System.IO, System.File
import StdEnv, ArgEnv
import Data.Error
import Control.Monad

import qualified Text

import Lamba

:: *CompilerState = { fileName :: String
				   , fileContents :: Maybe String
				   , errors :: [String]
				   , world :: !*World
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

compileError s = CIO \cs. (Error s, cs)

getFileName = CIO \cs=:{fileName}. (Ok fileName, cs)
getFileContents = CIO \cs=:{fileContents}. case fileContents of
	Nothing = (Error "No file contents!", cs)
	Just f = (Ok f, cs)

openFile :: CompilerIO String
openFile = CIO \cs. case readFile cs.fileName cs.world of
	(Error e, world) = (Error (toString e), {cs & world = world})
	(Ok contents, world) = (Ok contents, {cs & fileContents = Just contents, world = world})

initState filename world = {fileName = filename
						   , fileContents = Nothing
						   , errors = []
						   , world = world}

Start env = case main env of
	(Error e, env) = execIO (print e) env
	(Ok s, env) = execIO (print s) env
where
	main :: *World -> (MaybeError String String, *World)
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
		>>= \tokens. return ("Tokenizing succeeded")



