import Graphics.Gloss

main = animate (InWindow "Dragon" (500, 500) (0, 0)) white (dragonAnime (50,250) (450,250))

dragonAnime a b t = Line (dragon a b !! (round t `mod` 20))

alterne :: [Int] -> [Int]
alterne [] = []
alterne [x] = [x]
alterne (x:_:xs) = x:(alterne xs)

combine :: (a -> b -> c) -> [a] -> [b] -> [c]
combine _ [] ys = []
combine _ xs [] = []
combine f (x:xs) (y:ys) = (f x y):(combine f xs ys)

pasPascal :: [Int] -> [Int]
pasPascal xs = combine (+) (0:xs) (xs ++ [0])

pascal :: [[Int]]
pascal = iterate pasPascal [1]

pointAIntercaler :: Point -> Point -> Point
pointAIntercaler (x1,y1) (x2,y2) = (((x1 + x2)/2 + (y2 - y1)/2),((y2 + y1)/2 + (x1 - x2)/2))

pasDragon :: Path -> Path
pasDragon [x] = [x]
pasDragon [x,y] = [x,(pointAIntercaler x y),y]
pasDragon [x,y,z] = [x,(pointAIntercaler x y),y,(pointAIntercaler z y),z]
pasDragon (x:y:z:xs) = (pasDragon (x:y:z:[])) ++ (pasDragon (z:xs))

dragon :: Point -> Point -> [Path]
dragon x y = iterate pasDragon [x,y]
