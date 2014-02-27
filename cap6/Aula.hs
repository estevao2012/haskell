module Main where
	
	main :: IO()
	main = putStr( show(aux 2) ) 

	aux :: Int -> Int
	aux a = a+2 