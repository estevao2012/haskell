module Fatorial where
	
	fat :: Integer -> Integer
	fat n
		| n == 0 = 1
		| otherwise = n * fat(n-1)
		
	--fat 0 = 1
	--fat x = fat(x-1) * x