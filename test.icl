module test

import StdEnv, Data.Tuple, Data.Either, Text
import Lamba

import Lamba.Test.Tokenizer

:: TokenizerTest = TokenizerTest Int Int

Start
# tokenizerResults = map (appSnd3 tokenize) tokenizerTests
# failed = length (filter (\(name, got, expected). not (got == expected)) tokenizerResults)
# succeeded = length (filter (\(name, got, expected). got == expected) tokenizerResults)
# message = join "\n" (map formatTest tokenizerResults)
= message +++ if (failed > 0) "\nThere are test failures" "\nAll tests passed."

formatTest (name, result, expected)
| result == expected = "Passed: " +++ name
# prefix = "Failed: " +++ name
= case (expected, result) of
	(Left e1, Left e2) =  prefix + "\n\tExpected error: " + toString e1 + "\n\tGot error: " + toString e2
	(Right result, Left error) = prefix + "\n\tExpected result: [" + join ", " (map toString result) + "]\n\tGot error: " + toString error
	(Left error, Right result) = prefix + "\n\tExpected error: " + toString  error + "\n\tGot result: " + join ", " (map toString result)
	(Right r1, Right r2) = prefix + "\n\tExpected result: [" + join ", " (map toString r1) + "]\n\tGot result: [" + join ", " (map toString r2) + "]"



