module Pictures where

	type Picture = [[Char]]
	horse	:: Picture
	horse = [ "TESTE" , "NAO TESTE" ]
	black :: Picture
	black = ["########","############","########"]
	white :: Picture
	white = ["..........","..............",".........."]


	printPicture :: Picture -> IO()
	printPicture = putStr . concat . map (++"\n")

	flipH :: Picture -> Picture
	flipH = reverse

	flipV :: Picture -> Picture
	flipV = map reverse

	rotate :: Picture -> Picture
	rotate = flipH . flipV

	above :: Picture -> Picture -> Picture
	above = (++)

	beside :: Picture -> Picture -> Picture
	beside = zipWith (++)