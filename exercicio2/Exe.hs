module Exercicios where
	import Prelude hiding(elem,and,or)
	
	--Exercicios 5.7 - 5.8 - 5.20 - 5.21 - 5.26 
	--Exercicios 7.6 - 7.9 - 7.19 - 7.25 - 7.33
	--Exercicio 5.7
	data Shape = Circle Float 
							|Rectangle Float Float 
							|Triangle Float Float Float
							 deriving (Eq,Ord,Show)

	isRound :: Shape -> Bool
	isRound (Circle _) = True
	isRound (Rectangle _ _) = False
	isRound (Triangle _ _ _) = False

	area :: Shape -> Float
	area (Circle r) = pi*r*r
	area (Rectangle h w) = h*w
	area (Triangle a b c) = ( c * altura ) / 2
		where
			altura = sqrt( (b*b) + (a*a) )

	perimeter :: Shape -> Float
	perimeter (Circle x) = x
	perimeter (Rectangle h w ) = 2*h + 2*w
	perimeter (Triangle a b c) = a+b+c

	--Exercicios 5.8

	isSquare :: Float -> Float -> Bool
	isSquare a b
		| a == b = True
		| otherwise = False

	isEquilater :: Float -> Float -> Float -> Bool
	isEquilater a b c
		| (a == b) && (b == c) && (c == a) = True
		| otherwise = False

	isRegular :: Shape -> Bool
	isRegular (Circle _) = True
	isRegular (Rectangle h w) = isSquare h w
	isRegular (Triangle a b c) = isEquilater a b c

	--Exercicios 5.20
	isDivisor :: Integer -> Integer -> Bool
	isDivisor a b
		| a `mod` b == 0 = True
		| otherwise = False

	divisores :: Integer -> Integer -> [Integer]
	divisores a b
		| b > a = []
		| isDivisor a b == True = b : divisores a (b+1)
		| otherwise = divisores a (b+1)

	divisors :: Integer -> [Integer]
	divisors x = divisores x 1

	--Exercicios 5.21
	--Duas maneiras de fazer o exercicio
	--matches :: Integer -> [Integer] -> [Integer]
	--matches a b = filter (==a) b

	matches :: Integer -> [Integer] -> [Integer]
	matches a (x:b)
		| b == [] && x == a = [x]
		| b == [] && (x > a || x < a) = []
		| a == x = x : matches a b
		| otherwise = matches a b

	elem :: Integer -> [Integer] -> Bool
	elem a b
		| matches a b == [] = False
		| otherwise = True


	--Exercicio 5.26
	tabela = "n   fib n \n"
	--a = [1,2,3]
	fibo :: Integer -> Integer
	fibo n
		|n == 0 = 0
		|n == 1 = 1
		|otherwise = fibo (n-1) + fibo (n-2)

	montaTabela :: Integer -> Integer -> String
	montaTabela a b 
		| b == 0 = tabela ++ c ++ "   " ++ d ++ "\n" ++ montaTabela a (b+1)
		| a >= b = c ++ "   " ++ d ++ "\n" ++ montaTabela a (b+1)
		| otherwise = ""
		where
			c = show(b)
			d = show(fibo b)

	fibTable :: Integer -> String
	fibTable a = montaTabela a 0

	
	--Exercicio 7.6
	and :: [Bool] -> Bool
	and (a:b) = a && and b

	or :: [Bool] -> Bool
	or (a:b) = a || and b

	--Exercicio 7.9
	naoPertence :: Integer -> [Integer] -> Bool
	naoPertence x (a:b)
		| b == [] = True
		| a == x = False
		| otherwise = naoPertence x b

	unique :: [Integer] -> [Integer]
	unique xs = [ x | x <- xs , naoPertence x xs]

	--Exercicio 7.19
	iSort :: [(Int,Int)] -> [(Int,Int)]
	iSort [] = []
	iSort (x:xs) = ins x (iSort xs)

	ins :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
	ins x [] = [x]
	ins x (y:ys)
		| x <= y = x:y:ys
		| otherwise = y:ins x ys

	--Exercicio 7.25
	gSublists :: String -> String -> String
	gSublists "" "" = ""
	gSublists (x:xs) (y:ys) = [ a | a <- (x:xs) , b <- (y:ys) , a == b]

	sublist :: String -> String -> Bool
	sublist xs ys
		| gSublists xs ys == xs = True
		| otherwise = False

	subListOuSubsequencia :: String -> String -> String
	subListOuSubsequencia xs ys
		| sublist xs ys = "sublist"
		| otherwise = "subsequencia"

	--Exercicio 7.33
	isPalindrome :: String -> Bool
	isPalindrome (x : xs)
	  | xs == [] = True
	  | otherwise = x == last xs && isPalindrome (take ((length xs) - 1) xs)
	isPalindrome _ = True


