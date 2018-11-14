implementation module Lamba.Test.Tokenizer

import Data.Either

import Lamba.Tokenizer

tokenizerTests :: [(String, String, Either TokenizerError [(TokenizerLocation, Token)])]
tokenizerTests = operatorTests ++ identifierTests ++ numberTests ++ misc


operatorTests = [("PLUS", "+", Right [((1, 1), Symbol '+')])
				 , ("MINUS", "-", Right [((1, 1), Symbol '-')])
				 , ("TIMES", "*", Right [((1, 1), Symbol '*')])
				 , ("DIVIDE", "/", Right [((1, 1), Symbol '/')])
				 , ("EQUAL", "=", Right [((1, 1), Symbol '=')])
				 , ("COLON", ":", Right [((1, 1), Symbol ':')])
				 , ("GREATER", ">", Right [((1, 1), Symbol '>')])
				 , ("LESS", "<", Right [((1, 1), Symbol '<')])
				 ]

numberTests = [ ("NUM: 1", "1", Right [((1, 1), Number 1)])
			  , ("NUM: 1 1","1 1", Right [((1, 1), Number 1), ((1, 3), Number 1)])
			  , ("NUM: Illegal letter", "1a", Left (TokenizerError (1, 1) (IllegalCharacterDigit 'a') ))]

identifierTests = [ ("ID: Base", "test", Right [((1, 1), Identifier "test")])
				  , ("ID: End underscore", "test_ ", Right [((1, 1), Identifier "test_")])
				  , ("ID: End underscore space", "test_	", Right [((1, 1), Identifier "test_")])
				  , ("ID: End underscore tab", "test_	", Right [((1, 1), Identifier "test_")])
				  , ("ID: Multiple ", "test1 test2", Right [((1, 1), Identifier "test1"), ((1, 7), Identifier "test2")])
				  , ("ID: Start with _", "_test", Right [((1, 1), Identifier "_test")])
				  , ("ID: Random _", "_test_with_underscores_", Right [((1, 1), Identifier "_test_with_underscores_")])
				  , ("ID: Multiple newline", "test1\ntest2", Right [((1, 1), Identifier "test1"), ((2, 1), Identifier "test2")])
				 ]

misc = [("Space", " ", Right [])
		 , ("Tab", " ", Right [])]