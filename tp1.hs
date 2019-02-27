-- E:\Haskell\test.hs

tete :: [Int] -> Int
tete [] = error "liste vide"
tete (x:_) = x

reste :: [Int] -> [Int]
reste [] = error "liste vide"
reste (_:xs) = xs

longueur :: [a] -> Int
longueur []     = 0
longueur (_:xs) = 1 + longueur xs

liste_sommes :: [Int] -> [Int] ->[Int]
liste_sommes []     []     = []
liste_sommes (x:xs) (y:ys) = x+y : liste_sommes xs ys

add :: Num a => a -> a -> a
add x y = x + y

sommeDeXaY :: Int -> Int -> Int 
sommeDeXaY  x y = if x==y then x else x + (sommeDeXaY (x+1) y) 
 
somme :: [Int] -> Int
somme [] = 0
somme(x:xs) = x + somme xs

last' :: [Int] -> Int
last' [] = error "liste vide"
last'(x:[]) = x
last'(_:xs) = last' xs

init' :: [Int] -> [Int]
init' [] = error "liste vide"
init' (x:[]) =[]
init' (x:xs) = (x : init' xs)

init'' [] = error "l"
init'' xs = take ((length xs)-1) xs

(!!!) :: [a] -> Int -> a
(!!!) [] x = error "index too large"
(!!!) (a:xs) x = if x > length xs then error "index out of range" else if x == 0 then a
                else (!!!) xs (x-1)

(+++) :: [a] -> [a] -> [a]
(+++) [] [] = []
(+++) xs [] = xs
(+++) [] xs = xs
(+++) (x:[]) ys = (x:ys)
(+++) (x:xs) ys  = (+++) [x] ((+++) xs ys) 


concat'' :: [[a]] -> [a]
concat'' [] = []
concat'' ([]:ys) = concat'' ys
concat'' ((x:[]):ys) = x:(concat'' ys)
concat'' ((x:xs):ys) = x:(concat'' (xs:ys))

fc:: Int ->Int
fc x = x+1

map'' :: (a -> b) -> [a] -> [b]
map'' fct [] = []
map'' fct (x:xs) = (fct x):(map'' fct xs) 

fct:: (a -> a) -> a -> Integer -> [a]
fct f x 0 = [x]
fct f x n = x:(fct f (f x) (n-1) )

f2:: Integer -> [Int]
f2 n = (fct fc 0 n)