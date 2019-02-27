module Main where

import Graphics.Gloss

main = animate (InWindow "Dragon" (500, 500) (0, 0)) white (dragonAnime (50,250) (450,250))

dragonAnime a b t = Line (dragonOrdre  a b (round t `mod` 20))  --- deuxieme version

--- premiere version dragonAnime a b t = Line (dragon a b !! (round t `mod` 20))

pointAintercaler :: Point -> Point -> Point
pointAintercaler (xA,yA) (xB,yB) =  ((xA + xB)/2 + (yB - yA)/2, (yA + yB)/2 + (xA -xB)/2)



pasDragon :: Path -> Path
pasDragon [] = []
pasDragon [a] = [a]
pasDragon (a:(b:(c:xs))) = a : pointAintercaler a b : b :pointAintercaler c b : (pasDragon (c:xs))
pasDragon (a:(b:xs)) = a: pointAintercaler a b :pasDragon(b:xs)


dragon :: Point -> Point -> [Path]
dragon x y = iterate pasDragon [x,y]


dragonOrdre :: Point -> Point -> Int -> Path
dragonOrdre  (xA,yA) (xB,yB) 0 = (xA,yA):[ (xB,yB)]
dragonOrdre  (xA,yA) (xB,yB) n = (dragonOrdre (xA,yA) c (n-1) ) ++ (reverse (init ( dragonOrdre (xB,yB) c (n-1) ) ) )
								where c = pointAintercaler (xA,yA) (xB,yB)
                                

---stack ghc dragon.hs && ./dragon