sommeDeXaY :: Int -> Int -> Int
sommeDeXaY x y | x > y = 0
sommeDeXaY x y = x + sommeDeXaY (x+1) y

sommeList :: [Int] -> Int
sommeList [] = 0
sommeList (x:xs) = x + sommeList xs

newLast :: [t] -> t
newLast [] = error "Liste vide"
newLast x = head (reverse x)

newInit :: [t] -> [t]
newInit [] = error "Liste vide"
newInit x = reverse (tail (reverse x))

(!!!) :: [t] -> Int -> t
(!!!) _ y | y < 0 = error "Rang incorrect"
(!!!) (x:_) 0 = x
(!!!) (_:xs) y = (!!!) xs (y-1)

(+++) :: [t] -> [t] -> [t]
(+++) [] ys = ys
(+++) xs [] = xs
(+++) xs ys = (+++) (newInit xs) ((newLast xs):ys)

newConcat :: [[t]] -> [t]
newConcat [] = []
newConcat [xs] = xs
newConcat ((x:xs)) = (+++) x (newConcat xs)

newMap :: (a -> b) -> [a] -> [b]
newMap _ [] = []
newMap f (x:xs) = (f x):(newMap f xs)

-- Q7) "x = (!!) l" représente une fonction attendant un argument numérique représentant l'index de l'élément de la liste à isoler.

longueur :: [t] -> Int
longueur [] = 0
longueur (x:xs) = 1 + (longueur xs)

power :: (a -> a) -> a -> Int -> a
power _ x y | y < 0 = error "Puissance trop petite"
power _ x 0 = x
power f x y = f power f x (y-1)
