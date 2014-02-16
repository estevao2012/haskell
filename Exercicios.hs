module Exercicios where
	-- Todos exercicios 3.20 , 4.29 , 4.30 , 4.32
	
	--Exercício 3.20 
	avg3 :: Integer -> Integer -> Integer -> Float
	avg3 a b c = fromIntegral( a + (b + c) ) / fromIntegral(3)
	
	isAbove :: Integer -> Float -> Integer
	isAbove a avg
	 | fromIntegral(a) > avg = 1
	 | otherwise = 0

	howManyAboveArg :: Integer -> Integer -> Integer -> Integer
	howManyAboveArg a b c = (isAbove a x) + (isAbove b x) + (isAbove c x)
		where 
			x = avg3 a b c

	--Exercicio 4.29
	type Desenho = [[Char]]
 
	inserePonto :: Int -> [Char]
	inserePonto n
		| n == 0 = ""
		| otherwise = "." ++ inserePonto(n-1)


	percorreN :: Int -> [Char]
	percorreN n 
		| n > 2 = ['#'] ++ inserePonto( prev ) ++  ['#']
		| n == 2 = ['#','#']
		| otherwise = ['#']
		where
			prev = n-2

	criaSuperiorDesenho :: Int -> Int -> [Char]
	criaSuperiorDesenho n m 
		| n <= 0 = ""
		| otherwise = string ++ criaSuperiorDesenho aterior next 
		where
			aterior = n-2
			next = m+1
			string = inserePonto(m) ++ percorreN(n) ++ inserePonto(m) ++ "\n"

	geraDesenho :: Int -> Desenho
	geraDesenho n = words(criaSuperiorDesenho n 0)

	giraDesenho :: Desenho -> Desenho
	giraDesenho = reverse

	finalizaDesenho :: Desenho -> Desenho
	finalizaDesenho n = n ++ reverse n

	imprimeDesenho :: Int -> IO()
	imprimeDesenho = putStr . concat . map (++"\n") . finalizaDesenho . geraDesenho

	--Exercicio 4.30
	black = ["#"]
	white = ["."]

	above :: Desenho -> Desenho -> Desenho
	above = (++)

	beside :: Desenho -> Desenho -> Desenho
	beside = zipWith (++)

	blackLine:: Int -> Desenho
	blackLine n
		| n==1	  = black
		| n>1	  = black `beside` whiteLine (n-1)

	whiteLine:: Int -> Desenho
	whiteLine n
		| n==1	  = white
		| n>1	  = white `beside` blackLine (n-1)

	blackChess:: Int -> Int -> Desenho
	blackChess n m
		| n==1 = blackLine m
		| n>1	 = blackLine m `above` whiteChess (n-1) m

	whiteChess:: Int -> Int -> Desenho
	whiteChess n m
		| n==1 = whiteLine m
		| n>1	 = whiteLine m `above` blackChess (n-1) m

	chessBoard :: Int -> Desenho
	chessBoard n = blackChess n n


	--Exercicio 4.32

	divisao :: Integer -> Double
	divisao n
		| odd n == False = m
		| otherwise = (m - 1)
		where
			m = fromIntegral(n)/fromIntegral(2)


	powern :: Integer -> Integer
	powern n
		| odd n == False = (fromIntegral(2) ^ fromIntegral(m) ) ^ fromIntegral(2)
		| otherwise = ( (fromIntegral(2) ^ fromIntegral(m)) ^ fromIntegral(2) ) * fromIntegral(2)
		where
			m = ceiling(divisao n)