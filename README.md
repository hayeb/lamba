-- First: Only type-checking
-- Functional language
-- Pure language
-- Goal: Easy to work with, good error messages.
-- Standard operators: + - * / % (Int -> Int)
-- Whitespace independent
-- Pattern matching + algebraic datatypes
-- Polymorphism


Booleans: True | False
Strings: ""
Characters: 'c'


Hello world:
	Main = print "Hello, world!"

Factorial:
	fac :: Int -> Int
	fac 1 = 1
	fac n = n * fac (n - 1)

	Main = fac 15

Fibonacci:
	fib :: Int -> Int
	fib 0 = 1
	fib 1 = 1
	fib n = fin (n - 1) + fin (n - 2)

	Main = fib 15

Choices:
	choose :: Bool -> String
	choose bool
	| bool = "true"
	| !bool = "false"

	Main = choose True

Let:
	greet :: String -> String
	greet name
	# greetString = "Hello, " + name + "!"
	= greetString