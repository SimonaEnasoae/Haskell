--   
alterne :: [a] -> [a]
alterne [] = []
alterne (x:[]) = [x]
alterne (x:(y:ys)) = (x:alterne ys) 

fct :: Int ->Int -> Int
fct x y = x+y

combine :: (a -> b -> c) -> [a] -> [b] -> [c]
combine f [] [] = []
combine f (x:xs) (y:ys) = (f x y):(combine f xs ys)

pasPascal :: [Integer] -> [Integer]
pasPascal [x] = [x,x]
pasPascal xs = zipWith (+) ([0]++xs) (xs++[0])

pascal :: [[Integer]]
pascal =  iterate pasPascal [1]